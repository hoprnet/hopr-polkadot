/// A runtime module template with necessary imports

/// Feel free to remove or edit this file as needed.
/// If you change the name of this file, make sure to update its references in runtime/src/lib.rs
/// If you remove this file, you can remove those references


/// For more guidance on Substrate modules, see the example module
/// https://github.com/paritytech/substrate/blob/master/srml/example/src/lib.rs

use support::{decl_module, decl_storage, decl_event, ensure, StorageMap, dispatch::Result, traits::{Currency, ReservableCurrency}};
use runtime_primitives::traits::{Hash, Verify, CheckedDiv, As};
use system::{ensure_signed};
use parity_codec::{Encode, Decode};

use primitives::{sr25519::{Public, Signature}};

#[derive(Clone, PartialEq, Encode, Decode)]
#[cfg_attr(feature = "std", derive(Debug))]
enum ChannelState {
	UNINITIALIZED, // 0
	PARTYA_FUNDED, // 1
	PARTYB_FUNDED, // 2
	ACTIVE, // 3
	PENDING_SETTLEMENT // 4
}

impl Default for ChannelState {
	fn default() -> Self { ChannelState::UNINITIALIZED }
}

#[derive(Encode, Decode, Default, Clone, PartialEq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct Channel<Balance, Timestamp> {
	state: ChannelState,
	balance: Balance,
	balance_a: Balance,
	index: u16,
	settle_timestamp: Option<Timestamp>,
}

#[derive(Encode, Decode, Default, Clone, PartialEq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct State<Balance> {
	// number of open channels
	// Note: the smart contract doesn't know the actual
	//       channels but it knows how many open ones
	//       there are.
	openChannels: u16,
	stakedFunds: Balance
}
/// The module's configuration trait.
pub trait Trait: system::Trait + timestamp::Trait + balances::Trait  {
	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
	// type Currency: Currency<Self::AccountId> + ReservableCurrency<Self::AccountId>;
}

decl_storage! {
	trait Store for Module<T: Trait> as hopr {
		Channels get(channels): map T::Hash => Channel<T::Balance, T::Moment>;
		States get(state_of): map T::AccountId => State<T::Balance>;
		Nonces get(nonce_exists): map u32 => bool;
		AccountIdMap get(account_id): map Public => T::AccountId;
	}
}

decl_module! {
	/// The module declaration.
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event<T>() = default;

		pub fn create_funded(origin, nonce: u32, counterparty: Public, signature: Signature, funds: T::Balance, index: u64) -> Result {
			let sender = ensure_signed(origin)?;

			// Check that the nonce has not been used before.
			ensure!(!<Nonces<T>>::exists(nonce), "Nonce was already used.");
			<Nonces<T>>::insert(nonce, true);

			// @TODO add rest of the signed values
			let hashed_message = (nonce, index, funds).using_encoded(<T as system::Trait>::Hashing::hash);

			// Check signature
			ensure!(Signature::verify(&signature, hashed_message.as_ref(), &counterparty), "Signature must be valid.");

			// Check if user has enough funds
			ensure!(<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::can_reserve(&sender, funds.checked_div(&<T::Balance as As<u64>>::sa(2)).ok_or("underflow error")?), "User has not enough funds.");

			// Check if we know the counterparty
			ensure!(<AccountIdMap<T>>::exists(&counterparty), "We do not know the account Id of the counterparty.");
			
			let counterparty_account_id = Self::account_id(&counterparty);
			
			// Check if counterparty has enough funds
			ensure!(<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::can_reserve(&counterparty_account_id, funds.checked_div(&<T::Balance as As<u64>>::sa(2)).ok_or("underflow error")?), "Counterparty does not have enough funds.");

			let channel_id = Self::get_id(&counterparty_account_id, &sender);

			ensure!(!<Channels<T>>::exists(&channel_id), "Channel must not exist.");

			<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::reserve(&sender, funds.checked_div(&<T::Balance as As<u64>>::sa(2)).ok_or("underflow error")?)?;
			<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::reserve(&counterparty_account_id, funds.checked_div(&<T::Balance as As<u64>>::sa(2)).ok_or("underflow error")?)?;

			let balance: T::Balance = <T::Balance as As<u64>>::sa(2);
			let balance_a: T::Balance = <T::Balance as As<u64>>::sa(3);

			<Channels<T>>::insert(&channel_id, Channel {
				state: ChannelState::ACTIVE,
				balance,
				balance_a,
				index: 0,
				settle_timestamp: None,
			});

			Self::deposit_event(RawEvent::OpenedChannel(channel_id, balance, balance_a));

			Ok(())
		}
	}
}

decl_event!(
	pub enum Event<T> where 
		<T as system::Trait>::AccountId,
		<T as system::Trait>::Hash,
		<T as balances::Trait>::Balance {
		OpenedChannel(Hash, Balance, Balance),
		ClosedChannel(Hash, u16, Balance),
		OpenedChannelFor(AccountId, AccountId, Balance, Balance),
	}
);

impl<T: Trait> Module<T> {
	fn is_party_a(a: &T::AccountId, b: &T::AccountId) -> bool {
		a < b
	}

	/// Give the payment channels a meaningful ID that is the same for
	/// parties
	fn get_id(a: &T::AccountId, b: &T::AccountId) -> T::Hash {
		if Self::is_party_a(&a, &b) {
			(a, b).using_encoded(<T as system::Trait>::Hashing::hash)
		} else {
			(b, a).using_encoded(<T as system::Trait>::Hashing::hash)
		}
	}
}

/// tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use runtime_io::with_externalities;
	use primitives::{H256, Blake2Hasher};
	use support::{impl_outer_origin, assert_ok};
	use runtime_primitives::{
		BuildStorage,
		traits::{BlakeTwo256, IdentityLookup},
		testing::{Digest, DigestItem, Header}
	};

	impl_outer_origin! {
		pub enum Origin for Test {}
	}

	// For testing the module, we construct most of a mock runtime. This means
	// first constructing a configuration type (`Test`) which `impl`s each of the
	// configuration traits of modules we want to use.
	#[derive(Clone, Eq, PartialEq)]
	pub struct Test;
	impl system::Trait for Test {
		type Origin = Origin;
		type Index = u64;
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type Digest = Digest;
		type AccountId = u64;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type Event = ();
		type Log = DigestItem;
	}
	impl Trait for Test {
		type Event = ();
	}
	type hopr = Module<Test>;

	// This function basically just builds a genesis storage key/value store according to
	// our desired mockup.
	fn new_test_ext() -> runtime_io::TestExternalities<Blake2Hasher> {
		system::GenesisConfig::<Test>::default().build_storage().unwrap().0.into()
	}

	#[test]
	fn it_works_for_default_value() {
		with_externalities(&mut new_test_ext(), || {
			// Just a dummy test for the dummy funtion `do_something`
			// calling the `do_something` function with a value 42
			assert_ok!(hopr::do_something(Origin::signed(1), 42));
			// asserting that the stored value is equal to what we stored
			assert_eq!(hopr::something(), Some(42));
		});
	}
}

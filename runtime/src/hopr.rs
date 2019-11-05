/// A runtime module template with necessary imports

/// Feel free to remove or edit this file as needed.
/// If you change the name of this file, make sure to update its references in runtime/src/lib.rs
/// If you remove this file, you can remove those references


/// For more guidance on Substrate modules, see the example module
/// https://github.com/paritytech/substrate/blob/master/srml/example/src/lib.rs

use support::{decl_module, decl_storage, decl_event, ensure, StorageMap, dispatch::Result, traits::{ReservableCurrency}};
use runtime_primitives::traits::{Hash, Verify, CheckedAdd, CheckedSub, As};
use system::{ensure_signed};
use parity_codec::{Encode, Decode};

use primitives::{sr25519::{Public, Signature}};

#[derive(Clone, PartialEq, Encode, Decode)]
#[cfg_attr(feature = "std", derive(Debug))]
struct ChannelBalance<Balance> {
	balance: Balance,
	balance_a: Balance,
	index: u16
}

#[derive(Clone, PartialEq, Encode, Decode)]
#[cfg_attr(feature = "std", derive(Debug))]
pub enum Channel<Balance, Moment> {
	Uninitialized,
	Funded(ChannelBalance<Balance>),
	Active(ChannelBalance<Balance>),
	PendingSettlement(ChannelBalance<Balance>, Moment)
}

impl<Balance, Moment> Default for Channel<Balance, Moment> {
	fn default() -> Self { Self::Uninitialized }
}

#[derive(Encode, Decode, Default, Clone, PartialEq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct State<Balance, Hash> {
	// number of open channels
	// Note: the smart contract doesn't know the actual
	//       channels but it knows how many open ones
	//       there are.
	openChannels: u16,
	secret: Hash
}

pub type ChannelId<T> = <T as system::Trait>::Hash;

/// The module's configuration trait.
pub trait Trait: system::Trait + timestamp::Trait + balances::Trait {
	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
}

decl_storage! {
	trait Store for Module<T: Trait> as hopr {
		Channels get(channels): map ChannelId<T> => Channel<T::Balance, T::Moment>;
		States get(state_of): map T::AccountId => State<T::Balance, T::Hash>;
		Nonces get(nonce_exists): map T::Hash => bool;
		AccountIdMap get(pubkey): map T::AccountId => Public;
	}
}

decl_module! {
	/// The module declaration.
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event<T>() = default;

		pub fn create(origin, funds: T::Balance, counterparty: T::AccountId) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::can_reserve(&sender, funds), "User has not enough funds.");

			let channel_id = Self::get_id(&sender, &counterparty);

			let mut channel = Self::channels(channel_id);

			match channel {
				Channel::Uninitialized => {
					if Self::is_party_a(&sender, &counterparty) {
						channel = Channel::Funded(ChannelBalance {
							balance: funds,
							balance_a: funds,
							index: 0
						});
					} else {
						channel = Channel::Funded(ChannelBalance {
							balance: funds,
							balance_a: <T::Balance as As<u64>>::sa(0),
							index: 0
						});
					}
				},
				Channel::Funded(channel_balance) => {
					if Self::is_party_a(&sender, &counterparty) {
						channel = Channel::Funded(ChannelBalance {
							balance: channel_balance.balance.checked_add(&funds).ok_or("integer error")?,
							balance_a: channel_balance.balance_a.checked_add(&funds).ok_or("integer error")?,
							index: channel_balance.index
						});
					} else {
						channel = Channel::Funded(ChannelBalance {
							balance: channel_balance.balance.checked_add(&funds).ok_or("integer error")?,
							balance_a: channel_balance.balance_a.checked_sub(&funds).ok_or("integer error")?,
							index: channel_balance.index
						});
					}
				},
				_ => panic!("Channel is cannot be created twice.")
			}

			// ==== State change ================================
			<Channels<T>>::insert(channel_id, channel);
			<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::reserve(&sender, funds);
			
			Ok(())
		}

		pub fn setActive(origin, counterparty: T::AccountId, signature: Signature) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(<AccountIdMap<T>>::exists(&counterparty), "We do not know the public key of the counterparty.");
			
			let channel_id = Self::get_id(&sender, &counterparty);

			let mut channel = Self::channels(&channel_id);

			let counterparty_pubkey = Self::pubkey(counterparty);

			ensure!(Signature::verify(&signature, (channel).using_encoded(<T as system::Trait>::Hashing::hash).as_ref(), &counterparty_pubkey), "Invalid signature.");
		
			match channel {
				Channel::Funded(channel_balance) => {
					channel = Channel::Active(channel_balance);
				}
				_ => panic!("Channel does not exist and/or its state does not fit.")
			}

			// ==== State change ================================
			<Channels<T>>::insert(channel_id, channel);
			Self::test_and_set_nonce(<T as system::Trait>::Hashing::hash(signature.as_ref()))?;

			Self::deposit_event(RawEvent::OpenedChannel(channel_id, channel.channelbalance, balance_a));

			Ok(())
		}

		pub fn create_funded(origin, nonce: u32, counterparty: T::AccountId, signature: Signature, funds: T::Balance, index: u64) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(<AccountIdMap<T>>::exists(&counterparty), "We do not know the public key of the counterparty.");

			let channel_balance = ChannelBalance {
				balance: funds.checked_add(&funds).ok_or("integer error")?,
				balance_a: funds,
				index: 0
			};

			let mut channel = Channel::Funded(channel_balance);

			let counterparty_pubkey: Public = Self::pubkey(counterparty);

			let channel_id: ChannelId<T> = Self::get_id(&counterparty, &sender);

			ensure!(!<Channels<T>>::exists(&channel_id), "Channel must not exist.");

			ensure!(Signature::verify(&signature, (channel).using_encoded(<T as system::Trait>::Hashing::hash).as_ref(), &counterparty_pubkey), "Signature must be valid.");

			ensure!(<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::can_reserve(&sender, funds), "User does have not enough funds.");
			ensure!(<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::can_reserve(&counterparty, funds), "Counterparty does not have enough funds.");

			channel = Channel::Active(channel_balance);

			// ==== State change ================================
			Self::test_and_set_nonce(<T as system::Trait>::Hashing::hash(signature.as_ref()))?;

			<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::reserve(&sender, funds)?;
			<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::reserve(&counterparty, funds)?;

			<Channels<T>>::insert(channel_id, channel);

			Self::deposit_event(RawEvent::OpenedChannel(channel_id, channel_balance.balance, channel_balance.balance_a));

			Ok(())
		}

		pub fn setSecret(origin, hash: T::Hash) {
			<States<T>>::update 
		}

		pub fn withdraw(origin, nonce: u32, index: u16, balance: T::Balance, balanceA: T::Balance, signature: Signature) -> Result {
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
	fn test_and_set_nonce(nonce: T::Hash) -> Result {
		ensure!(!<Nonces<T>>::exists(nonce), "Nonce was already used.");
		<Nonces<T>>::insert(nonce, true);
		Ok(())
	}

	fn is_party_a(a: &T::AccountId, b: &T::AccountId) -> bool {
		a < b
	}

	/// Give the payment channels a meaningful ID that is the same for
	/// parties
	fn get_id(a: &T::AccountId, b: &T::AccountId) -> ChannelId<T> {
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

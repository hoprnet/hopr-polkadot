/// A runtime module template with necessary imports

/// Feel free to remove or edit this file as needed.
/// If you change the name of this file, make sure to update its references in runtime/src/lib.rs
/// If you remove this file, you can remove those references


/// For more guidance on Substrate modules, see the example module
/// https://github.com/paritytech/substrate/blob/master/srml/example/src/lib.rs

use support::{decl_module, decl_storage, decl_event, StorageValue, dispatch::Result, traits::{Currency, ReservableCurrency}};
use runtime_primitives::traits::Hash;
use system::ensure_signed;
use parity_codec::{Encode, Decode};

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
struct Channel<Balance, Timestamp> {
	state: ChannelState,
	balance: Balance,
	balanceA: Balance,
	index: u16,
	settleTimestamp: Timestamp,
}

    // // Open channels
    // mapping(bytes32 => Channel) public channels;

    // struct State {
    //     bool isSet;
    //     // number of open channels
    //     // Note: the smart contract doesn't know the actual
    //     //       channels but it knows how many open ones
    //     //       there are.
    //     uint256 openChannels;
    //     uint256 stakedEther;
    // }

    // // Keeps track of the states of the
    // // participating parties.
    // mapping(address => State) public states;

    // // Keeps track of the nonces that have
    // // been used to open payment channels
    // mapping(bytes16 => bool) public nonces;

/// The module's configuration trait.
pub trait Trait: system::Trait + timestamp::Trait {
	// TODO: Add other types and constants required configure this module.

	/// The overarching event type.
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
	type Currency: Currency<Self::AccountId> + ReservableCurrency<Self::AccountId>;
}

type BalanceOf<T> = <<T as Trait>::Currency as Currency<<T as system::Trait>::AccountId>>::Balance;

decl_storage! {
	trait Store for Module<T: Trait> as hopr {
		// Just a dummy storage item. 
		// Here we are declaring a StorageValue, `Something` as a Option<u32>
		// `get(something)` is the default getter which returns either the stored `u32` or `None` if nothing stored
		Something get(something): Option<u32>;
		Channels: map T::AccountId => Channel<BalanceOf<T>, T::Moment>;
	}
}

decl_module! {
	/// The module declaration.
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		// Initializing events
		// this is needed only if you are using events in your module
		fn deposit_event<T>() = default;

		// Just a dummy entry point.
		// function that can be called by the external world as an extrinsics call
		// takes a parameter of the type `AccountId`, stores it and emits an event
		pub fn do_something(origin, something: u32) -> Result {
			// TODO: You only need this if you want to check it was signed.
			let who = ensure_signed(origin)?;

			// TODO: Code to execute when something calls this.
			// For example: the following line stores the passed in u32 in the storage
			<Something<T>>::put(something);

			// here we are raising the Something event
			//Self::deposit_event(RawEvent::SomethingStored(something, who));
			Ok(())
		}

		pub fn stakeFor() {
			
		}

    // function stakeFor(address beneficiary) payable public {
    //     if (msg.value > 0) {
    //         if (!states[beneficiary].isSet) {
    //             states[beneficiary] = State(true, 0, msg.value);
    //         } else {
    //             states[beneficiary].stakedEther = states[beneficiary].stakedEther.add(uint256(msg.value));
    //         }
    //     }
    // }   
	}
}

decl_event!(
	pub enum Event<T> where 
		<T as system::Trait>::AccountId,
		<T as system::Trait>::Hash,
		Balance = BalanceOf<T> {
		OpenedChannel(Hash, Balance, Balance),
		ClosedChannel(Hash, u16, Balance),
		OpenedChannelFor(AccountId, AccountId, Balance, Balance),
	}
);

impl<T: Trait> Module<T> {
	fn isPartyA(a: &T::AccountId, b: &T::AccountId) -> bool {
		a < b
	}

	/// Give the payment channels a meaningful ID that is the same for
	/// parties
	fn getId(a: T::AccountId, b: T::AccountId) -> T::Hash {
		if Self::isPartyA(&a, &b) {
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

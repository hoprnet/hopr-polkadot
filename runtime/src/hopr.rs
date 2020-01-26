use parity_codec::{Decode, Encode};
use primitives::{
	sr25519::{Public, Signature},
	H256,
};
use runtime_primitives::traits::{As, CheckedAdd, CheckedSub, Hash, Verify};
/// A runtime module template with necessary imports

/// Feel free to remove or edit this file as needed.
/// If you change the name of this file, make sure to update its references in runtime/src/lib.rs
/// If you remove this file, you can remove those references

/// For more guidance on Substrate modules, see the example module
/// https://github.com/paritytech/substrate/blob/master/srml/example/src/lib.rs
use support::{
	decl_event, decl_module, decl_storage, dispatch::Result, ensure, traits::ReservableCurrency,
	StorageMap,
};

use system::ensure_signed;

/// Length of the pending_window in seconds
const PENDING_WINDOW: u64 = 1 * 10;

#[derive(Clone, PartialEq, Encode, Decode)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct ChannelBalance<Balance> {
	balance: Balance,
	balance_a: Balance,
}

#[derive(Clone, PartialEq, Encode, Decode)]
#[cfg_attr(feature = "std", derive(Debug))]
pub enum Channel<Balance, Moment> {
	Uninitialized,
	Funded(ChannelBalance<Balance>),
	Active(ChannelBalance<Balance>),
	PendingSettlement(ChannelBalance<Balance>, Moment),
}

impl<Balance, Moment> Default for Channel<Balance, Moment> {
	fn default() -> Self {
		Self::Uninitialized
	}
}

#[derive(Encode, Decode, Default, Clone, PartialEq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct State<Hash, Public> {
	secret: Hash,
	pubkey: Public,
}

#[derive(Encode, Decode, Default, Clone, PartialEq)]
#[cfg_attr(feature = "std", derive(Debug))]
pub struct LotteryTicket<Hash, Balance> {
	challenge: Hash,
	on_chain_secret: Hash,
	amount: Balance,
	win_prob: Hash,
}

// #[derive(Encode, Decode, Default, Clone, PartialEq)]
// #[cfg_attr(feature = "std", derive(Debug))]
// pub struct SignedLotteryTicket<Hash, Balance, Signature> {
// 	lottery_ticket: LotteryTicket<Hash, Balance>,
// 	signature: Signature,
// }

pub type ChannelId<T> = <T as system::Trait>::Hash;
pub type PreImage<T> = <T as system::Trait>::Hash;

/// The module's configuration trait.
pub trait Trait: system::Trait + timestamp::Trait + balances::Trait {
	type Event: From<Event<Self>> + Into<<Self as system::Trait>::Event>;
	// type AccountId: From<<Self as Trait>::AccountId> + Into<<Self as system::Trait>::AccountId> + From<<Self as system::Trait>::AccountId> + Into<Public>;
}

decl_storage! {
	trait Store for Module<T: Trait> as hopr {
		Channels get(channels): map ChannelId<T> => Channel<T::Balance, T::Moment>;
		States get(state): map T::AccountId => State<T::Hash, Public>;
		Nonces get(nonce_exists): map T::Hash => bool;
		PendingWindow get(pending_window): u64 = PENDING_WINDOW;
	}
}

// @TODO epochs for reset on-chain secret
// @TODO increase epoch after initiating settlement

decl_module! {
	/// Module that process Hopr payments
	pub struct Module<T: Trait> for enum Call where origin: T::Origin {
		fn deposit_event<T>() = default;

		/// Initialises a payment channel between two parties.
		pub fn create(origin, funds: T::Balance, counterparty: T::AccountId) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(sender != counterparty, "Sender and counterparty must not be the same.");
			ensure!(funds > <T::Balance as As<u64>>::sa(0), "Funds must be strictly greater than zero.");

			ensure!(<States<T>>::exists(&sender), "Party must have called init() before.");
			ensure!(<States<T>>::exists(&counterparty), "Party must have called init() before.");

			ensure!(<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::can_reserve(&sender, funds), "User has not enough funds.");

			let channel_id = Self::get_id(&sender, &counterparty);

			let channel_balance = match Self::channels(channel_id) {
				Channel::Uninitialized => {
					if Self::is_party_a(&sender, &counterparty) {
						ChannelBalance {
							balance: funds,
							balance_a: funds,
						}
					} else {
						ChannelBalance {
							balance: funds,
							balance_a: <T::Balance as As<u64>>::sa(0),
						}
					}
				},
				Channel::Funded(channel_balance) => {
					if Self::is_party_a(&sender, &counterparty) {
						ChannelBalance {
							balance: channel_balance.balance.checked_add(&funds).ok_or("integer error")?,
							balance_a: channel_balance.balance_a.checked_add(&funds).ok_or("integer error")?,
						}
					} else {
						ChannelBalance {
							balance: channel_balance.balance.checked_add(&funds).ok_or("integer error")?,
							balance_a: channel_balance.balance_a,
						}
					}
				},
				_ => return Err("Channel is must not be created twice."),
			};

			// ==== State change ================================
			<Channels<T>>::insert(channel_id, Channel::Funded(channel_balance.clone()));
			<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::reserve(&sender, funds)?;

			Self::deposit_event(RawEvent::Funded(sender, channel_balance.balance, channel_balance.balance_a));

			Ok(())
		}

		/// Turns a previously funded channel into an active one.
		pub fn set_active(origin, counterparty: T::AccountId, signature: Signature) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(sender != counterparty, "Sender and counterparty must not be the same.");

			ensure!(<States<T>>::exists(&sender), "Party must have called init() before.");
			ensure!(<States<T>>::exists(&counterparty), "Party must have called init() before.");

			let channel_id = Self::get_id(&sender, &counterparty);
			ensure!(<Channels<T>>::exists(&channel_id), "Channel does not exist and/or its state does not fit.");

			let channel_balance = match Self::channels(&channel_id) {
				Channel::Funded(channel_balance) => channel_balance,
				_ => return Err("Channel does not exist and/or its state does not fit.")
			};

			let counterparty_pubkey = Self::state(counterparty).pubkey;
			// println!("{:?}", counterparty_pubkey);
			// println!("{:?}", (Channel::Funded(channel_balance.clone()) as Channel<T::Balance, T::Moment>).encode().as_slice());
			ensure!(Signature::verify(&signature, (Channel::Funded(channel_balance.clone()) as Channel<T::Balance, T::Moment>).encode().as_slice(), &counterparty_pubkey), "Invalid signature.");

			// ==== State change ================================
			Self::test_and_set_nonce(<T as system::Trait>::Hashing::hash(signature.as_ref()))?;

			<Channels<T>>::insert(channel_id, Channel::Active(channel_balance.clone()));

			Self::deposit_event(RawEvent::Opened(channel_id, channel_balance.balance, channel_balance.balance_a));

			Ok(())
		}

		/// Initialises a channel that is funded by both sides and turn it immediately into
		/// an active one.
		pub fn create_funded(origin, counterparty: T::AccountId, signature: Signature, funds: T::Balance) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(sender != counterparty, "Sender and counterparty must not be the same.");
			ensure!(funds > <T::Balance as As<u64>>::sa(0), "Funds must be strictly greater than zero.");

			ensure!(<States<T>>::exists(&counterparty), "We do not know the public key of the counterparty.");

			let channel_balance: ChannelBalance<T::Balance> = ChannelBalance {
				balance: funds.checked_add(&funds).ok_or("integer error")?,
				balance_a: funds,
			};

			let counterparty_pubkey: Public = Self::state(&counterparty).pubkey;

			let channel_id: ChannelId<T> = Self::get_id(&counterparty, &sender);

			ensure!(!<Channels<T>>::exists(&channel_id), "Channel must not exist.");

			ensure!(Signature::verify(&signature, (Channel::Funded(channel_balance.clone()) as Channel<T::Balance, T::Moment>).encode().as_slice(), &counterparty_pubkey), "Signature must be valid.");

			ensure!(<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::can_reserve(&sender, funds), "User does have not enough funds.");
			ensure!(<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::can_reserve(&counterparty, funds), "Counterparty does not have enough funds.");

			// ==== State change ================================
			Self::test_and_set_nonce(<T as system::Trait>::Hashing::hash(signature.as_ref()))?;

			<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::reserve(&sender, funds)?;
			<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::reserve(&counterparty, funds)?;

			<Channels<T>>::insert(channel_id, Channel::Active(channel_balance.clone()));

			Self::deposit_event(RawEvent::Opened(channel_id, channel_balance.balance, channel_balance.balance_a));

			Ok(())
		}

		/// Resets the stored on-chain secret.
		pub fn set_secret(origin, hash: T::Hash) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(<States<T>>::exists(&sender), "Call init() before setting a new on-chain secret.");

			ensure!(Self::state(&sender).secret != hash, "New and old hash must not be the same.");

			// ==== State change ================================
			<States<T>>::mutate(&sender, |state| {
				state.secret = hash;
			});

			Ok(())
		}

		/// Initialises the stored on-chain data.
		pub fn init(origin, pubkey: H256, hash: T::Hash) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(!<States<T>>::exists(&sender), "State must be set at most once.");

			// ==== State change ================================
			<States<T>>::insert(&sender, State {
				pubkey: Public::from_raw(*pubkey.as_fixed_bytes()),
				secret: hash,
			});

			Ok(())
		}

		/// Redeems a previously issued ticket.
		pub fn redeem_ticket(origin, signature: Signature, counterparty: T::AccountId, pre_image: PreImage<T>, s_a: PreImage<T>, s_b: PreImage<T>, amount: T::Balance, win_prob: T::Hash) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(sender != counterparty, "Sender and counterparty must not be the same.");

			ensure!(<States<T>>::exists(&sender), "Party must have called init() before.");
			ensure!(<States<T>>::exists(&counterparty), "Party must have called init() before.");

			ensure!(amount > <T::Balance as As<u64>>::sa(0), "Amount must be strictly greater than zero.");

			let on_chain_secret = Self::state(&sender).secret;
			ensure!(<T as system::Trait>::Hashing::hash(pre_image.as_ref()) == on_chain_secret, "Given value is not a pre-image of the stored on-chain secret");

			let counterparty_pubkey = Self::state(&counterparty).pubkey;

			let channel_id = Self::get_id(&sender, &counterparty);

			let mut channel_balance = match Self::channels(channel_id) {
				Channel::PendingSettlement(_, timestamp) if timestamp::Module::<T>::now() > timestamp => return Err("Ticket redemption must have happened before end of pending window."),
				Channel::PendingSettlement(channel_balance, _) |  Channel::Active(channel_balance) => channel_balance,
				_ => return Err("Channel does not exist and/or its state does not fit."),
			};

			if Self::is_party_a(&sender, &counterparty) {
				ensure!(channel_balance.balance_a.checked_add(&amount).ok_or("Integer error.")? <= channel_balance.balance, "Transferred funds must not exceed channel balance.");
			} else {
				ensure!(channel_balance.balance_a.checked_sub(&amount).ok_or("Integer error.")? >= <<T as balances::Trait>::Balance as As<u64>>::sa(0), "Transferred funds must not exceed channel balance.");
			}

			let hashed_s_a = <T as system::Trait>::Hashing::hash(s_a.as_ref());
			let hashed_s_b = <T as system::Trait>::Hashing::hash(s_b.as_ref());

			let challenge = (hashed_s_a, hashed_s_b).using_encoded(<T as system::Trait>::Hashing::hash);

			let ticket: LotteryTicket<T::Hash, T::Balance> = LotteryTicket {
				challenge,
				on_chain_secret,
				amount,
				win_prob
			};
			let hashed_ticket = ticket.using_encoded(<T as system::Trait>::Hashing::hash);

			ensure!(Self::cmp_hash(&hashed_ticket, &win_prob), "Ticket must be a win.");

			ensure!(Verify::verify(&signature, ticket.encode().as_slice(), &counterparty_pubkey), "Signature must be valid.");

			// ==== Prepare state change=========================
			if Self::is_party_a(&sender, &counterparty) {
				channel_balance.balance_a = channel_balance.balance_a.checked_add(&amount).ok_or("Integer error.")?;
			} else {
				channel_balance.balance_a = channel_balance.balance_a.checked_sub(&amount).ok_or("Integer error.")?;
			}

			// ==== State change ================================
			Self::test_and_set_nonce(<T as system::Trait>::Hashing::hash(signature.as_ref()))?;

			<States<T>>::mutate(&sender, |state| {
				state.secret = pre_image;
			});

			<Channels<T>>::mutate(&channel_id, |channel| {
				*channel = match channel {
					Channel::Active(_) => Channel::Active(channel_balance),
					Channel::PendingSettlement(_, timestamp) => {
						Self::deposit_event(RawEvent::PushedBackSettlement(channel_id, channel_balance.balance_a));
						Channel::PendingSettlement(channel_balance, timestamp.clone())
					},
					_ => return Err("Channel does not exist and/or its state does not fit."),
				};
				Ok(())
			})?;

			Ok(())
		}

		pub fn redeem_aggregated_tickets() -> Result {
			Err("Not implemented")
		}

		/// Restores the channel to a previously agreed backup state.
		pub fn initiate_recovery(origin, counterparty: T::AccountId, signature: Signature, claimed_channel_balance: ChannelBalance<T::Balance>) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(sender != counterparty, "Sender and counterparty must not be the same.");

			ensure!(<States<T>>::exists(&sender), "Party must have called init() before.");
			ensure!(<States<T>>::exists(&counterparty), "Party must have called init() before.");

			let channel_id = Self::get_id(&sender, &counterparty);

			let channel_balance = match Self::channels(channel_id) {
				Channel::Active(channel_balance) | Channel::Funded(channel_balance) => channel_balance,
				_ => return Err("Channel does not exist and/or its state does not fit."),
			};

			ensure!(channel_balance == claimed_channel_balance, "Channel must be in the agreed state.");

			let counterparty_pubkey = Self::state(&counterparty).pubkey;

			let message = ("restore_transaction", channel_id, claimed_channel_balance).encode();
			ensure!(Signature::verify(&signature, message.as_slice(), &counterparty_pubkey), "Signature must be valid.");

			// ==== State change ================================
			let end_of_pending_window = timestamp::Module::<T>::now().checked_add(&<T::Moment as As<u64>>::sa(Self::pending_window())).ok_or("Integer error")?;
			<Channels<T>>::mutate(&channel_id, |channel| {
				*channel = Channel::PendingSettlement(channel_balance.clone(), end_of_pending_window);
			});

			Self::deposit_event(RawEvent::InitiatedSettlement(channel_id, channel_balance.balance_a));

			Ok(())
		}

		pub fn initiate_settlement(origin, counterparty: T::AccountId) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(sender != counterparty, "Sender and counterparty must not be the same.");

			let channel_id = Self::get_id(&sender, &counterparty);

			let channel_balance = match Self::channels(channel_id) {
				Channel::Active(channel_balance) => channel_balance,
				_ => return Err("Channel does not exist and/or its state does not fit."),
			};

			// ==== State change ================================
			let end_of_pending_window = timestamp::Module::<T>::now().checked_add(&<T::Moment as As<u64>>::sa(Self::pending_window())).ok_or("Integer error")?;
			<Channels<T>>::insert(channel_id, Channel::PendingSettlement(channel_balance.clone(), end_of_pending_window));

			Self::deposit_event(RawEvent::InitiatedSettlement(channel_id, channel_balance.balance_a));

			Ok(())
		}

		pub fn withdraw(origin, counterparty: T::AccountId) -> Result {
			// ==== Verification ================================
			let sender = ensure_signed(origin)?;

			ensure!(sender != counterparty, "Sender and counterparty must not be the same.");

			ensure!(<States<T>>::exists(&sender), "Party must have called init() before.");
			ensure!(<States<T>>::exists(&counterparty), "Party must have called init() before.");

			let channel_id = Self::get_id(&sender, &counterparty);
			let channel = Self::channels(&channel_id);

			let channel_balance = match channel {
				Channel::PendingSettlement(_, timestamp) if timestamp::Module::<T>::now() < timestamp => return Err("Channel is not yet recoverable."),
				Channel::PendingSettlement(channel_balance, _) => channel_balance,
				_ => return Err("Channel does not exist and/or its state does not fit."),
			};

			// ==== State change ================================
			if Self::is_party_a(&sender, &counterparty) {
				<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::unreserve(&sender, channel_balance.balance_a);
				<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::unreserve(&counterparty, channel_balance.balance.checked_sub(&channel_balance.balance_a).ok_or("Integer error")?);
			} else {
				<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::unreserve(&sender, channel_balance.balance.checked_sub(&channel_balance.balance_a).ok_or("Integer error")?);
				<balances::Module<T> as ReservableCurrency<<T as system::Trait>::AccountId>>::unreserve(&counterparty, channel_balance.balance_a);
			}

			<Channels<T>>::remove(channel_id);

			Ok(())
		}
	}
}

decl_event!(
	pub enum Event<T> where 
		<T as system::Trait>::AccountId,
		<T as system::Trait>::Hash,
		<T as balances::Trait>::Balance {
		/// Channel got funded.
		Funded(AccountId, Balance, Balance),
		/// Channel is now open.
		Opened(Hash, Balance, Balance),
		/// Settlement was initiated.
		InitiatedSettlement(Hash, Balance),
		/// Settlement was delayed.
		PushedBackSettlement(Hash, Balance),
		/// Payment channel opened by third party.
		OpenedFor(AccountId, AccountId, Balance, Balance),
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

	/// Give the payment channels a meaningful ID that is the same for both
	/// parties
	fn get_id(a: &T::AccountId, b: &T::AccountId) -> ChannelId<T> {
		if Self::is_party_a(&a, &b) {
			(a, b).using_encoded(<T as system::Trait>::Hashing::hash)
		} else {
			(b, a).using_encoded(<T as system::Trait>::Hashing::hash)
		}
	}

	fn cmp_hash(first_hash: &T::Hash, second_hash: &T::Hash) -> bool {
		*(first_hash.as_ref()) < *(second_hash.as_ref())
	}
}

/// tests for this module
#[cfg(test)]
mod tests {
	use super::*;

	use primitives::{sr25519, Blake2Hasher, Hasher, Pair, H256};
	use runtime_io::{with_externalities, TestExternalities};
	use runtime_primitives::{
		testing::{Digest, DigestItem, Header},
		traits::{BlakeTwo256, IdentityLookup, Verify},
		BuildStorage,
	};
	use support::{assert_noop, assert_ok, impl_outer_origin};

	type AccountId = <AccountSignature as Verify>::Signer;
	type AccountSignature = sr25519::Signature;

	impl_outer_origin! {
		pub enum Origin for HoprTest {}
	}

	// For testing the module, we construct most of a mock runtime. This means
	// first constructing a configuration type (`Test`) which `impl`s each of the
	// configuration traits of modules we want to use.
	#[derive(Clone, Eq, PartialEq)]
	pub struct HoprTest;
	impl system::Trait for HoprTest {
		type Origin = Origin;
		type Index = u64;
		type BlockNumber = u64;
		type Hash = H256;
		type Hashing = BlakeTwo256;
		type Digest = Digest;
		type AccountId = AccountId;
		type Lookup = IdentityLookup<Self::AccountId>;
		type Header = Header;
		type Event = ();
		type Log = DigestItem;
	}

	impl timestamp::Trait for HoprTest {
		/// A timestamp: seconds since the unix epoch.
		type Moment = u64;
		type OnTimestampSet = ();
	}

	impl balances::Trait for HoprTest {
		/// The type for recording an account's balance.
		type Balance = u128;
		/// What to do if an account's free balance gets zeroed.
		type OnFreeBalanceZero = ();
		/// What to do if a new account is created.
		type OnNewAccount = ();
		/// The uniquitous event type.
		type Event = ();

		type TransactionPayment = ();
		type DustRemoval = ();
		type TransferPayment = ();
	}

	// impl sudo::Trait for HoprTest {
	// 	type Event = ();
	// 	type Proposal = ();
	// }

	impl super::Trait for HoprTest {
		type Event = ();
	}

	type Hopr = Module<HoprTest>;

	fn new_test_ext() -> TestExternalities<Blake2Hasher> {
		let alice = account_key("Alice");
		let bob = account_key("Bob");

		let mut t = system::GenesisConfig::<HoprTest>::default()
			.build_storage()
			.unwrap()
			.0;
		t.extend(
			balances::GenesisConfig::<HoprTest> {
				transaction_base_fee: 0,
				transaction_byte_fee: 0,
				existential_deposit: 0,
				transfer_fee: 0,
				creation_fee: 0,
				balances: vec![alice, bob]
					.iter()
					.cloned()
					.map(|k| (k, 1 << 60))
					.collect(),
				vesting: vec![],
			}
			.build_storage()
			.unwrap()
			.0,
		);
		t.into()
	}

	const PRE_IMAGE: [u8; 32] = [0u8; 32];

	fn account_key(s: &str) -> AccountId {
		sr25519::Pair::from_string(&format!("//{}", s), None)
			.expect("static values are valid; qed")
			.public()
	}

	fn my_account_key(seed: [u8; 32]) -> AccountId {
		println!("{:?}", sr25519::Pair::from_seed(seed).public());
		sr25519::Pair::from_seed(seed).public()
	}

	fn key(s: &str) -> sr25519::Pair {
		sr25519::Pair::from_string(&format!("//{}", s), None).expect("static values are valid; qed")
	}

	#[test]
	fn verify_init() {
		with_externalities(&mut new_test_ext(), || {
			let account_id = my_account_key([
				179, 250, 241, 51, 4, 74, 235, 236, 189, 136, 113, 169, 32, 129, 135, 131, 248,
				227, 232, 9, 164, 37, 241, 49, 4, 100, 146, 146, 81, 16, 235, 192,
			]);
			let sender = Origin::signed(account_id.clone());

			let hashed_secret = <Blake2Hasher as Hasher>::hash(&PRE_IMAGE);

			assert_ok!(Hopr::init(
				sender.clone(),
				account_id.clone().into(),
				hashed_secret.clone()
			));

			assert_eq!(
				Hopr::state(account_id.clone()),
				State {
					pubkey: account_id.clone(),
					secret: hashed_secret
				}
			);

			assert_noop!(
				Hopr::init(
					sender.clone(),
					account_id.into(),
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
				),
				"State must be set at most once."
			);
		});
	}

	#[test]
	fn verify_set_secret() {
		with_externalities(&mut new_test_ext(), || {
			let account_id = account_key("Alice");
			let sender = Origin::signed(account_id.clone());

			let first_hash = <Blake2Hasher as Hasher>::hash(&PRE_IMAGE);
			let second_hash = <Blake2Hasher as Hasher>::hash(first_hash.as_ref());

			assert_noop!(
				Hopr::set_secret(sender.clone(), second_hash.clone()),
				"Call init() before setting a new on-chain secret."
			);

			assert_ok!(Hopr::init(
				sender.clone(),
				account_id.into(),
				first_hash.clone()
			));

			assert_ok!(Hopr::set_secret(sender.clone(), second_hash.clone()));

			assert_noop!(
				Hopr::set_secret(sender, second_hash),
				"New and old hash must not be the same."
			);
		})
	}

	#[test]
	fn verify_create() {
		with_externalities(&mut new_test_ext(), || {
			let account_id = account_key("Alice");
			let account_id_counterparty = account_key("Bob");

			let sender = Origin::signed(account_id.clone());
			let counterparty = Origin::signed(account_id_counterparty.clone());

			assert_noop!(
				Hopr::create(sender.clone(), 1, account_id_counterparty.clone()),
				"Party must have called init() before."
			);
			assert_noop!(
				Hopr::create(counterparty.clone(), 1, account_id.clone()),
				"Party must have called init() before."
			);

			assert_noop!(
				Hopr::create(sender.clone(), 1, account_id.clone()),
				"Sender and counterparty must not be the same."
			);

			assert_ok!(Hopr::init(
				sender.clone(),
				account_id.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));
			assert_ok!(Hopr::init(
				counterparty.clone(),
				account_id_counterparty.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));

			assert_ok!(Hopr::create(
				sender.clone(),
				1,
				account_id_counterparty.clone()
			));
			let channel_id = Hopr::get_id(&account_id, &account_id_counterparty);
			assert_eq!(
				Hopr::channels(channel_id),
				Channel::Funded(ChannelBalance {
					balance: 1,
					balance_a: 0,
				})
			);

			assert_ok!(Hopr::create(
				sender.clone(),
				1,
				account_id_counterparty.clone()
			));

			assert_eq!(
				Hopr::channels(channel_id),
				Channel::Funded(ChannelBalance {
					balance: 2,
					balance_a: 0,
				})
			);

			assert_ok!(Hopr::create(counterparty.clone(), 1, account_id.clone()));

			assert_eq!(
				Hopr::channels(channel_id),
				Channel::Funded(ChannelBalance {
					balance: 3,
					balance_a: 1,
				})
			);
		})
	}

	#[test]
	fn verify_create_funded() {
		with_externalities(&mut new_test_ext(), || {
			let account_id = account_key("Alice");
			let account_id_counterparty = account_key("Bob");

			let sender = Origin::signed(account_id.clone());
			let counterparty = Origin::signed(account_id_counterparty.clone());

			assert_noop!(
				Hopr::create(sender.clone(), 1, account_id_counterparty.clone()),
				"Party must have called init() before."
			);
			assert_noop!(
				Hopr::create(counterparty.clone(), 1, account_id.clone()),
				"Party must have called init() before."
			);

			assert_noop!(
				Hopr::create(sender.clone(), 1, account_id.clone()),
				"Sender and counterparty must not be the same."
			);
			assert_noop!(
				Hopr::create(sender.clone(), 0, account_id_counterparty.clone()),
				"Funds must be strictly greater than zero."
			);

			assert_ok!(Hopr::init(
				sender.clone(),
				account_id.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));
			assert_ok!(Hopr::init(
				counterparty.clone(),
				account_id_counterparty.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));
			let channel_balance: ChannelBalance<u128> = ChannelBalance {
				balance: 2,
				balance_a: 1,
			};

			let signature = key("Bob").sign(
				(Channel::Funded(channel_balance.clone()) as Channel<u128, u64>)
					.encode()
					.as_slice(),
			);

			assert_ok!(Hopr::create_funded(
				sender.clone(),
				account_id_counterparty.clone(),
				signature.clone(),
				1
			));

			let channel_id = Hopr::get_id(&account_id, &account_id_counterparty);

			assert_eq!(Hopr::channels(channel_id), Channel::Active(channel_balance));

			assert_noop!(
				Hopr::create_funded(
					sender.clone(),
					account_id_counterparty.clone(),
					signature.clone(),
					1
				),
				"Channel must not exist."
			);
		})
	}

	#[test]
	fn verify_set_active() {
		with_externalities(&mut new_test_ext(), || {
			let account_id = account_key("Alice");
			let account_id_counterparty = account_key("Bob");

			let sender = Origin::signed(account_id.clone());
			let counterparty = Origin::signed(account_id_counterparty.clone());

			let channel_balance: ChannelBalance<u128> = ChannelBalance {
				balance: 1,
				balance_a: 0,
			};
			let signature = key("Bob").sign(
				(Channel::Funded(channel_balance.clone()) as Channel<u128, u64>)
					.encode()
					.as_slice(),
			);
			let signature_sender = key("Alice").sign(
				(Channel::Funded(channel_balance.clone()) as Channel<u128, u64>)
					.encode()
					.as_slice(),
			);

			assert_noop!(
				Hopr::set_active(
					counterparty.clone(),
					account_id_counterparty.clone(),
					signature.clone()
				),
				"Sender and counterparty must not be the same."
			);

			assert_noop!(
				Hopr::set_active(
					sender.clone(),
					account_id_counterparty.clone(),
					signature.clone()
				),
				"Party must have called init() before."
			);
			assert_noop!(
				Hopr::set_active(
					counterparty.clone(),
					account_id.clone(),
					signature_sender.clone()
				),
				"Party must have called init() before."
			);

			assert_ok!(Hopr::init(
				sender.clone(),
				account_id.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));
			assert_ok!(Hopr::init(
				counterparty.clone(),
				account_id_counterparty.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));

			assert_noop!(
				Hopr::set_active(
					sender.clone(),
					account_id_counterparty.clone(),
					signature.clone()
				),
				"Channel does not exist and/or its state does not fit."
			);

			assert_ok!(Hopr::create(
				sender.clone(),
				1,
				account_id_counterparty.clone()
			));

			let channel_id = Hopr::get_id(&account_id, &account_id_counterparty);

			assert_eq!(
				Hopr::channels(channel_id),
				Channel::Funded(channel_balance.clone())
			);

			assert_ok!(Hopr::set_active(
				sender.clone(),
				account_id_counterparty.clone(),
				signature.clone()
			));

			assert_noop!(
				Hopr::create(sender.clone(), 1, account_id_counterparty.clone()),
				"Channel is must not be created twice."
			);

			assert_eq!(
				Hopr::channels(channel_id),
				Channel::Active(channel_balance.clone())
			);

			assert_noop!(
				Hopr::set_active(
					sender.clone(),
					account_id_counterparty.clone(),
					signature.clone()
				),
				"Channel does not exist and/or its state does not fit."
			);

			assert_eq!(Hopr::channels(channel_id), Channel::Active(channel_balance));
		})
	}

	#[test]
	fn verify_initiate_recovery_and_withdraw() {
		with_externalities(&mut new_test_ext(), || {
			let account_id = account_key("Alice");
			let account_id_counterparty = account_key("Bob");

			let sender = Origin::signed(account_id.clone());
			let counterparty = Origin::signed(account_id_counterparty.clone());

			assert_ok!(Hopr::init(
				sender.clone(),
				account_id.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));
			assert_ok!(Hopr::init(
				counterparty.clone(),
				account_id_counterparty.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));

			assert_ok!(Hopr::create(
				sender.clone(),
				1,
				account_id_counterparty.clone()
			));
			let channel_balance: ChannelBalance<u128> = ChannelBalance {
				balance: 1,
				balance_a: 0,
			};

			let channel_id = Hopr::get_id(&account_id, &account_id_counterparty);

			let opening_signature = key("Bob").sign(
				(Channel::Funded(channel_balance.clone()) as Channel<u128, u64>)
					.encode()
					.as_slice(),
			);

			assert_ok!(Hopr::set_active(
				sender.clone(),
				account_id_counterparty.clone(),
				opening_signature.clone()
			));

			let message = ("restore_transaction", channel_id, &channel_balance).encode();

			let recovery_signature = key("Bob").sign(message.as_slice());

			assert_ok!(Hopr::initiate_recovery(
				sender.clone(),
				account_id_counterparty.clone(),
				recovery_signature.clone(),
				channel_balance.clone()
			));

			assert_noop!(
				Hopr::initiate_recovery(
					sender.clone(),
					account_id_counterparty.clone(),
					recovery_signature.clone(),
					channel_balance.clone()
				),
				"Channel does not exist and/or its state does not fit."
			);

			let timestamp = timestamp::Module::<HoprTest>::now()
				.checked_add(Hopr::pending_window())
				.unwrap();
			assert_eq!(
				Hopr::channels(channel_id.clone()),
				Channel::PendingSettlement(channel_balance.clone(), timestamp)
			);

			assert_noop!(
				Hopr::withdraw(sender.clone(), account_id_counterparty.clone()),
				"Channel is not yet recoverable."
			);

			assert_ok!(Hopr::dispatch(
				timestamp::Call::<HoprTest>::set(timestamp),
				Origin::INHERENT
			));

			assert_ok!(Hopr::withdraw(
				sender.clone(),
				account_id_counterparty.clone()
			));

			assert_eq!(Hopr::channels(channel_id.clone()), Channel::Uninitialized);

			assert_noop!(
				Hopr::withdraw(sender.clone(), account_id_counterparty.clone()),
				"Channel does not exist and/or its state does not fit."
			);
		})
	}

	#[test]
	fn verify_redeem_ticket_and_withdraw() {
		with_externalities(&mut new_test_ext(), || {
			let account_id = account_key("Alice");
			let account_id_counterparty = account_key("Bob");

			let sender = Origin::signed(account_id.clone());
			let counterparty = Origin::signed(account_id_counterparty.clone());

			assert_ok!(Hopr::init(
				sender.clone(),
				account_id.clone().into(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));

			let counterparty_on_chain_secret =
				<Blake2Hasher as Hasher>::hash(<Blake2Hasher as Hasher>::hash(&PRE_IMAGE).as_ref());
			assert_ok!(Hopr::init(
				counterparty.clone(),
				account_id_counterparty.clone().into(),
				counterparty_on_chain_secret
			));

			assert_ok!(Hopr::create(
				sender.clone(),
				1,
				account_id_counterparty.clone()
			));
			let channel_balance: ChannelBalance<u128> = ChannelBalance {
				balance: 1,
				balance_a: 0,
			};

			let channel_id = Hopr::get_id(&account_id, &account_id_counterparty);

			let opening_signature = key("Bob").sign(
				(Channel::Funded(channel_balance.clone()) as Channel<u128, u64>)
					.encode()
					.as_slice(),
			);

			let s_a: [u8; 32] = [3u8; 32];
			let s_b: [u8; 32] = [4u8; 32];
			let hashed_s_a = <Blake2Hasher as Hasher>::hash(&s_a);
			let hashed_s_b = <Blake2Hasher as Hasher>::hash(&s_b);

			let challenge = (hashed_s_a, hashed_s_b).using_encoded(<Blake2Hasher as Hasher>::hash);
			let ticket: LotteryTicket<H256, u128> = LotteryTicket {
				challenge,
				on_chain_secret: counterparty_on_chain_secret,
				amount: 1,
				win_prob: <Blake2Hasher as Hasher>::hash(&PRE_IMAGE),
			};
			let redeem_signature = key("Alice").sign(ticket.encode().as_slice());

			assert_noop!(
				Hopr::redeem_ticket(
					counterparty.clone(),
					redeem_signature.clone(),
					account_id.clone(),
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE),
					H256::from(s_a.clone()),
					H256::from(s_b.clone()),
					1,
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
				),
				"Channel does not exist and/or its state does not fit."
			);

			assert_ok!(Hopr::set_active(
				sender.clone(),
				account_id_counterparty.clone(),
				opening_signature.clone()
			));

			assert_eq!(
				Hopr::channels(channel_id.clone()),
				Channel::Active(channel_balance.clone())
			);

			assert_noop!(
				Hopr::redeem_ticket(
					counterparty.clone(),
					redeem_signature.clone(),
					account_id.clone(),
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE),
					H256::from(s_a.clone()),
					H256::from(s_b.clone()),
					2,
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
				),
				"Transferred funds must not exceed channel balance."
			);

			assert_noop!(
				Hopr::redeem_ticket(
					counterparty.clone(),
					opening_signature.clone(),
					account_id.clone(),
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE),
					H256::from(s_a.clone()),
					H256::from(s_b.clone()),
					1,
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
				),
				"Signature must be valid."
			);

			assert_ok!(Hopr::redeem_ticket(
				counterparty.clone(),
				redeem_signature.clone(),
				account_id.clone(),
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE),
				H256::from(s_a.clone()),
				H256::from(s_b.clone()),
				1,
				<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
			));

			assert_noop!(
				Hopr::redeem_ticket(
					counterparty.clone(),
					redeem_signature.clone(),
					account_id.clone(),
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE),
					H256::from(s_a.clone()),
					H256::from(s_b.clone()),
					1,
					<Blake2Hasher as Hasher>::hash(&PRE_IMAGE)
				),
				"Given value is not a pre-image of the stored on-chain secret"
			);

			let new_channel_balance: ChannelBalance<u128> = ChannelBalance {
				balance: 1,
				balance_a: 1,
			};

			assert_eq!(
				Hopr::channels(channel_id.clone()),
				Channel::Active(new_channel_balance.clone())
			);

			assert_ok!(Hopr::initiate_settlement(
				sender.clone(),
				account_id_counterparty.clone()
			));

			let timestamp = timestamp::Module::<HoprTest>::now()
				.checked_add(Hopr::pending_window())
				.unwrap();
			assert_eq!(
				Hopr::channels(channel_id.clone()),
				Channel::PendingSettlement(new_channel_balance.clone(), timestamp.clone())
			);

			assert_noop!(
				Hopr::withdraw(sender.clone(), account_id_counterparty.clone()),
				"Channel is not yet recoverable."
			);

			assert_ok!(Hopr::dispatch(
				timestamp::Call::<HoprTest>::set(timestamp),
				Origin::INHERENT
			));

			assert_ok!(Hopr::withdraw(
				sender.clone(),
				account_id_counterparty.clone()
			));
		})
	}
}

use hopr_polkadot_runtime::{
	AccountId, BalancesConfig, ConsensusConfig, GenesisConfig, IndicesConfig, SudoConfig,
	TimestampConfig,
};
use primitives::{ed25519, sr25519, Pair};
use substrate_service;

use ed25519::Public as AuthorityId;

// Note this is the URL for the telemetry server
//const STAGING_TELEMETRY_URL: &str = "wss://telemetry.polkadot.io/submit/";

/// Specialized `ChainSpec`. This is a specialization of the general Substrate ChainSpec type.
pub type ChainSpec = substrate_service::ChainSpec<GenesisConfig>;

/// The chain specification option. This is expected to come in from the CLI and
/// is little more than one of a number of alternatives which can easily be converted
/// from a string (`--chain=...`) into a `ChainSpec`.
#[derive(Clone, Debug)]
pub enum Alternative {
	/// Whatever the current runtime is, with just Alice as an auth.
	Development,
	/// Whatever the current runtime is, with simple Alice/Bob auths.
	LocalTestnet,
}

fn authority_key(s: &str) -> AuthorityId {
	ed25519::Pair::from_string(&format!("//{}", s), None)
		.expect("static values are valid; qed")
		.public()
}

fn _account_key(s: &str) -> AccountId {
	sr25519::Pair::from_string(&format!("//{}", s), None)
		.expect("static values are valid; qed")
		.public()
}

fn my_account_key(seed: [u8; 32]) -> AccountId {
	println!("{:?}", sr25519::Pair::from_seed(seed).public());
	sr25519::Pair::from_seed(seed).public()
}

impl Alternative {
	/// Get an actual chain config from one of the alternatives.
	pub(crate) fn load(self) -> Result<ChainSpec, String> {
		Ok(match self {
			Alternative::Development => ChainSpec::from_genesis(
				"Development",
				"dev",
				|| {
					testnet_genesis(
						vec![authority_key("Alice")],
						vec![
							// Alice
							my_account_key([
								179, 250, 241, 51, 4, 74, 235, 236, 189, 136, 113, 169, 32, 129,
								135, 131, 248, 227, 232, 9, 164, 37, 241, 49, 4, 100, 146, 146, 81,
								16, 235, 192,
							]),
							// Bob
							my_account_key([
								92, 184, 239, 54, 33, 217, 77, 221, 42, 146, 115, 208, 8, 121, 247,
								77, 22, 150, 119, 174, 141, 58, 197, 86, 220, 44, 141, 25, 78, 125,
								133, 200,
							]),
							// Charlie
							my_account_key([
								6, 65, 105, 137, 215, 135, 117, 129, 163, 126, 249, 126, 14, 39,
								82, 123, 17, 57, 246, 246, 58, 66, 192, 87, 230, 15, 140, 69, 170,
								80, 17, 169,
							]),
							// Dave
							my_account_key([
								113, 155, 140, 235, 183, 41, 119, 207, 73, 191, 154, 176, 163, 187,
								131, 252, 23, 40, 32, 211, 203, 180, 232, 142, 111, 133, 132, 121,
								246, 193, 228, 212,
							]),
							// Ed
							my_account_key([
								94, 136, 136, 113, 183, 90, 5, 193, 102, 64, 43, 51, 74, 231, 121,
								188, 160, 234, 98, 115, 116, 154, 36, 220, 52, 5, 27, 102, 141,
								253, 233, 169,
							]),
							// Fred
							my_account_key([
								94, 136, 136, 113, 183, 90, 5, 193, 102, 64, 43, 51, 74, 231, 121,
								188, 160, 234, 98, 115, 116, 154, 36, 220, 52, 5, 27, 102, 141,
								253, 233, 169,
							]),
							// George
							my_account_key([
								66, 176, 165, 38, 87, 35, 196, 6, 36, 89, 13, 175, 226, 10, 123,
								214, 71, 119, 2, 207, 31, 101, 233, 72, 6, 15, 84, 64, 74, 60, 123,
								92,
							]),
						],
						// Alice
						my_account_key([
							179, 250, 241, 51, 4, 74, 235, 236, 189, 136, 113, 169, 32, 129, 135,
							131, 248, 227, 232, 9, 164, 37, 241, 49, 4, 100, 146, 146, 81, 16, 235,
							192,
						]),
					)
				},
				vec![],
				None,
				None,
				None,
				None,
			),
			Alternative::LocalTestnet => ChainSpec::from_genesis(
				"Local Testnet",
				"local_testnet",
				|| {
					testnet_genesis(
						vec![authority_key("Alice"), authority_key("Bob")],
						vec![
							// Alice
							my_account_key([
								179, 250, 241, 51, 4, 74, 235, 236, 189, 136, 113, 169, 32, 129,
								135, 131, 248, 227, 232, 9, 164, 37, 241, 49, 4, 100, 146, 146, 81,
								16, 235, 192,
							]),
							// Bob
							my_account_key([
								92, 184, 239, 54, 33, 217, 77, 221, 42, 146, 115, 208, 8, 121, 247,
								77, 22, 150, 119, 174, 141, 58, 197, 86, 220, 44, 141, 25, 78, 125,
								133, 200,
							]),
							// Charlie
							my_account_key([
								6, 65, 105, 137, 215, 135, 117, 129, 163, 126, 249, 126, 14, 39,
								82, 123, 17, 57, 246, 246, 58, 66, 192, 87, 230, 15, 140, 69, 170,
								80, 17, 169,
							]),
							// Dave
							my_account_key([
								113, 155, 140, 235, 183, 41, 119, 207, 73, 191, 154, 176, 163, 187,
								131, 252, 23, 40, 32, 211, 203, 180, 232, 142, 111, 133, 132, 121,
								246, 193, 228, 212,
							]),
							// Ed
							my_account_key([
								94, 136, 136, 113, 183, 90, 5, 193, 102, 64, 43, 51, 74, 231, 121,
								188, 160, 234, 98, 115, 116, 154, 36, 220, 52, 5, 27, 102, 141,
								253, 233, 169,
							]),
							// Fred
							my_account_key([
								94, 136, 136, 113, 183, 90, 5, 193, 102, 64, 43, 51, 74, 231, 121,
								188, 160, 234, 98, 115, 116, 154, 36, 220, 52, 5, 27, 102, 141,
								253, 233, 169,
							]),
							// George
							my_account_key([
								66, 176, 165, 38, 87, 35, 196, 6, 36, 89, 13, 175, 226, 10, 123,
								214, 71, 119, 2, 207, 31, 101, 233, 72, 6, 15, 84, 64, 74, 60, 123,
								92,
							]),
						],
						// Alice
						my_account_key([
							179, 250, 241, 51, 4, 74, 235, 236, 189, 136, 113, 169, 32, 129, 135,
							131, 248, 227, 232, 9, 164, 37, 241, 49, 4, 100, 146, 146, 81, 16, 235,
							192,
						]),
					)
				},
				vec![],
				None,
				None,
				None,
				None,
			),
		})
	}

	pub(crate) fn from(s: &str) -> Option<Self> {
		match s {
			"dev" => Some(Alternative::Development),
			"" | "local" => Some(Alternative::LocalTestnet),
			_ => None,
		}
	}
}

fn testnet_genesis(
	initial_authorities: Vec<AuthorityId>,
	endowed_accounts: Vec<AccountId>,
	root_key: AccountId,
) -> GenesisConfig {
	GenesisConfig {
		consensus: Some(ConsensusConfig {
			code: include_bytes!("../runtime/wasm/target/wasm32-unknown-unknown/release/hopr_polkadot_runtime_wasm.compact.wasm").to_vec(),
			authorities: initial_authorities.clone(),
		}),
		system: None,
		timestamp: Some(TimestampConfig {
			minimum_period: 5, // 10 second block time.
		}),
		indices: Some(IndicesConfig {
			ids: endowed_accounts.clone(),
		}),
		balances: Some(BalancesConfig {
			transaction_base_fee: 1,
			transaction_byte_fee: 0,
			existential_deposit: 500,
			transfer_fee: 0,
			creation_fee: 0,
			balances: endowed_accounts.iter().cloned().map(|k|(k, 1 << 60)).collect(),
			vesting: vec![],
		}),
		sudo: Some(SudoConfig {
			key: root_key,
		}),
	}
}

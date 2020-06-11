# HOPR net

HOPR is a privacy-preserving messaging protocol that incentivizes users to participate in the network. It provides privacy by relaying messages via several relay nodes to the recipient. Relay nodes are getting paid via payment channels for their services.

## hopr-polkadot

A SRML module whose purpose is to process payments of **[HOPRnet](https://hoprnet.io)** as a parachain of Polkadot.

For more details about **[HOPRnet](https://hoprnet.io)** and its integration into the Polkadot ecosystem.

## Building

Install Rust:

```bash
curl https://sh.rustup.rs -sSf | sh
```

Install required tools:

```bash
./scripts/init.sh
```

Build the WebAssembly binary:

```bash
./scripts/build.sh
```

Build all native code:

```bash
cargo build
# get some coffee ...
```

## Testing

You can run the unit tests with:

```bash
cargo test -p hopr-polkadot-runtime
```

## Run a parachain node

You can start a development chain with:

```bash
cargo run -- --dev
```

## Run Hopr

```bash
# node hopr --network=polkadot
```

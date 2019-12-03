import { exec } from 'child_process'

export function startTestnet() {
  exec('cargo run -- --dev')
}

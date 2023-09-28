use aiken/bytearray
use aiken/cbor
use aiken/hash.{Blake2b_224, Hash}
use aiken/list
use aiken/transaction.{InlineDatum, Output, ScriptContext, Transaction}
use aiken/transaction/credential.{Credential, ScriptCredential}
use aiken/transaction/value.{Value}
use randr/prng.{hex_to_dec, icg_sequence, range_sequence, to_bytearray}

type Script =
  ByteArray

type VerificationKey =
  ByteArray

type PublicKeyHash =
  Hash<Blake2b_224, VerificationKey>

type PolicyId =
  Hash<Blake2b_224, Script>

type PaymentCredential =
  Credential

type State {
  block_number: Int,
  current_hash: ByteArray,
  leading_zeros: Int,
  difficulty_number: Int,
  epoch_time: Int,
  current_posix_time: Int,
  extra: Data,
  interlink: List<Data>,
}

validator {
  fn mint(_rdm: Bool, ctx: ScriptContext) -> Bool {
    // Set tunita vars...
    let tuna_address: PaymentCredential = ScriptCredential(#"")
    let tuna_policyid: PolicyId = #""
    let tuna_asset_name: ByteArray = #""
    let donation_address: ByteArray = #""

    // 
    let ScriptContext { transaction, .. } = ctx

    // Get the minted value and the tuna datum as reference input. 
    let Transaction { reference_inputs, outputs, mint, .. } = transaction
    // Expect a list with just one element from the reference input
    expect [ref_input] = reference_inputs
    let Output { address, value, datum, .. } = ref_input.output

    // Cond 1: Does the address of the reference input matches the tuna script address?
    let is_tuna_address: Bool = address.payment_credential == tuna_address

    // Extract current hash and posix time from the tuna script datum
    expect InlineDatum(state_datum) = datum
    expect State { current_hash, current_posix_time, .. }: State = state_datum

    // Concat time and current hash to use as a digest, then convert it to decimal.
    let time = cbor.serialise(current_posix_time)
    let digest =
      bytearray.concat(time, current_hash)
        |> hash.sha2_256()
        |> hex_to_dec()

    // Take the digest as a PRNG seed.
    let random_name =
      icg_sequence([digest], 33)
        |> list.drop(1)
        |> range_sequence(0, 255)
        |> to_bytearray(#"", _)

    // Extract the minted value that correspond to the random assetname.
    expect [(_, token_name, minted_amount)] =
      value.from_minted_value(mint)
        |> value.flatten()
        |> list.filter(fn(match) { match.2nd == random_name })

    // Get value to paid to donation address
    let paid_to_donation: Value = value_paid_to(outputs, donation_address)

    // Check if 10 tunitas were sent to donation address.
    let tuna_amount =
      value.quantity_of(paid_to_donation, tuna_policyid, tuna_asset_name)

    and {
      is_tuna_address,
      token_name == random_name,
      minted_amount == 1,
      tuna_amount == 10,
    }
  }
}

// Helper functions

// Get value paid to a public key hash.
fn value_paid_to(ouputs: List<Output>, receiver: PublicKeyHash) -> Value {
  let receiver_address = credential.from_verification_key(receiver)
  expect [match] = list.filter(ouputs, fn(o) { o.address == receiver_address })
  match.value
}

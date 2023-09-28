use aiken/builtin
use aiken/bytearray
use aiken/list

// Helper functions

// From hex to decimal
pub fn hex_to_dec(input: ByteArray) -> Int {
  bytearray.foldl(input, 0, fn(byte, acc) { acc * 256 + byte })
}

// From decimal to bytearray
pub fn to_bytearray(bs: ByteArray, input: List<Int>) -> ByteArray {
  when input is {
    [] -> bs
    [x, ..] -> {
      let result = bytearray.push(bs, x)
      let next_input = list.drop(input, 1)
      to_bytearray(result, next_input)
    }
  }
}

// Range a element between the min value to max value
fn to_range(input: Int, min_value: Int, max_value: Int) -> Int {
  let range_length = max_value - min_value + 1
  input % range_length + min_value
}

// Range a sequence between the min and max value.
pub fn range_sequence(
  sequence: List<Int>,
  min_value: Int,
  max_value: Int,
) -> List<Int> {
  list.map(sequence, to_range(_, min_value, max_value))
}

// PRNGs

// Linear congruential generator
fn linear_congruential_gen(input: Int) -> Int {
  ( 49235258628958 * input + 0 ) % 281474976710597
  //( 3 * input + 13 ) % 1024
}

fn lcg_sequence(input: List<Int>, range: Int) -> List<Int> {
  expect Some(last) = list.last(input)
  let state = linear_congruential_gen(last)
  let result = list.concat(input, [state])
  let r = range - 1
  when r is {
    0 -> input
    _ -> lcg_sequence(result, r)
  }
}

//Inverse congruential generator 

//const p = 9223372036854775783
//const a = 5520335699031059059
//const b = 2752743153957480735

const p = 2147483647

const a = 21916

const b = 71499791

fn inverse_congruential_gen(input: Int) -> Int {
  if input == 0 {
    b
  } else {
    let (_, _, i) = euclides_integer(p, input)
    ( a * i + b ) % p
  }
  //( 1664525 * i + 1013904223 ) % 4294967296
}

fn gdc(a: Int, b: Int, s1: Int, t1: Int, s2: Int, t2: Int) -> (Int, Int, Int) {
  let remainder = a % b
  let result = a / b
  let s3 = s1 - result * s2
  let t3 = t1 - result * t2

  if remainder == 0 {
    (b, s2, t2)
  } else {
    gdc(b, remainder, s2, t2, s3, t3)
  }
}

fn euclides_integer(a: Int, b: Int) -> (Int, Int, Int) {
  let (r, s, t) = gdc(a, b, 1, 0, 0, 1)
  if r < 0 {
    (r * -1, s * -1, t * -1)
  } else {
    (r, s, t)
  }
}

pub fn icg_sequence(input: List<Int>, range: Int) -> List<Int> {
  expect Some(last) = list.last(input)
  let state = inverse_congruential_gen(last)
  let result = list.concat(input, [state])
  let r = range - 1
  when r is {
    0 -> input
    _ -> icg_sequence(result, r)
  }
}

// Liner shiftback register

fn shift(input: ByteArray) -> ByteArray {
  let ultimate_byte = builtin.index_bytearray(input, 31)
  let init = builtin.slice_bytearray(0, 31, input)
  builtin.cons_bytearray(ultimate_byte, init)
}

fn to_bin(n: Int, bits: List<Int>) -> List<Int> {
  if n == 0 {
    bits
  } else {
    let q = n / 2
    let r = n % 2
    let result = list.push(bits, r)
    to_bin(q, result)
  }
}

//fn to_bool(input: List<Int>) -> List<Bool> {
//  list.map(input, fn(x) { x == 1 })
//}

fn arithmetic_xor(a: Int, b: Int) -> Int {
  a + b - a * b * ( 1 + a + b - a * b )
}

fn xor(a: Bool, b: Bool) -> Bool {
  !(a && b) && ( a || b )
}

fn linear_shiftback(
  seed: List<Bool>,
  counter: Int,
  sequence: List<Bool>,
) -> List<Bool> {
  when counter is {
    0 -> sequence
    _ -> {
      expect (init, [ultimate]) = list.span(seed, 254)
      expect Some(penultimate) = list.last(init)
      let xor_step = xor(penultimate, ultimate)
      let shift_step = list.push(init, xor_step)
      let result = list.push(sequence, xor_step)
      linear_shiftback(shift_step, counter - 1, result)
    }
  }
}

// Tests Helper functions

test xor_arith() {
  arithmetic_xor(1, 1) == 0
}

test hex_to_dec_1() {
  hex_to_dec(
    #"00000000002e6e47898fdd2e82d32a3ac99393310caccb88cac3aacb45264317",
  ) == 19100514641295229236530139513041640718376034630276887559115522839
}

test hex_to_dec_2() {
  hex_to_dec(#"64") == 100
}

test hex_to_dec_3() {
  hex_to_dec(#"0a") == 10
}

test hex_to_dec_4() {
  hex_to_dec(#"0f") == 15
}

test hex_to_dec_5() {
  hex_to_dec(
    #"00000000001b0c73760c00008f38a6088e4d1a46a9f94a0d0d2eb7ea698a5876",
  ) == 11127163779649771764236953377282915902155323388856262444771334262
}

test hex_to_dec_6() {
  hex_to_dec(#"0aaa") == 2730
}

test shift_1() {
  shift(#"00000000002e6e47898fdd2e82d32a3ac99393310caccb88cac3aacb45264317") == #"1700000000002e6e47898fdd2e82d32a3ac99393310caccb88cac3aacb452643"
}
// Test PRNGs

//test lcg_6() fail {
//  let rand_seq =
//    lcg_sequence(
//      [11127163779649771764236953377282915902155323388856262444771334262],
//      32,
//    )
//  let byte_array = list.map(rand_seq, to_range(_, 0, 256))
//  byte_array == [255]
//  //let byte_array = list.map(rand_seq, to_range(_)) |> to_bytearray(#"", _)
//  // byte_array == #[255]
//}

//test lcg_7() fail {
//  let rand_seq =
//    lcg_sequence(
//      [1112716377964977176423695337728291590215532338885113131123100],
//      33,
//    )
//  let rand_array = list.map(rand_seq, to_range(_, 0, 256))
//  //let byte_array = list.map(rand_seq, to_range(_)) |> to_bytearray(#"", _)
//  rand_array == [255]
//  //byte_array == #""
//}

//test linear_shiftback_1() {
//  let hash =
//    #"41dd04e47a6906ea7641e7073c048e3195de1037d83adc87647c0e845ff5004b"
//      |> hex_to_dec()
//      |> to_bin([])
//      |> to_bool()
//  linear_shiftback(hash, 10, []) == [True]
//}

pub type Datum {
  accumulator: ByteArray,
}

pub type Redeemer {
  setup: List<ByteArray>,
  proof: ByteArray,
}

let encode_rune buf cp =
  if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
  else if cp < 0x800 then begin
    Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else if cp < 0x10000 then begin
    Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end
  else begin
    Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
  end

let rune_to_string cp =
  let buf = Buffer.create 4 in
  encode_rune buf cp;
  Buffer.contents buf

let byte_length b =
  if b < 0x80 then 1
  else if b land 0xE0 = 0xC0 then 2
  else if b land 0xF0 = 0xE0 then 3
  else 4

let decode_next s i =
  let b = Char.code s.[i] in
  if b < 0x80 then (b, 1)
  else if b land 0xE0 = 0xC0 then
    (((b land 0x1F) lsl 6) lor (Char.code s.[i + 1] land 0x3F), 2)
  else if b land 0xF0 = 0xE0 then
    ( ((b land 0x0F) lsl 12)
      lor ((Char.code s.[i + 1] land 0x3F) lsl 6)
      lor (Char.code s.[i + 2] land 0x3F),
      3 )
  else
    ( ((b land 0x07) lsl 18)
      lor ((Char.code s.[i + 1] land 0x3F) lsl 12)
      lor ((Char.code s.[i + 2] land 0x3F) lsl 6)
      lor (Char.code s.[i + 3] land 0x3F),
      4 )

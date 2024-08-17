module [read]

# https://gbdev.io/pandocs/The_Cartridge_Header.html

Feature : [Ram, Battery, Timer, Rumble, Sensor]
Type := [
    Rom (List Feature),
    Mbc1 (List Feature),
    Mbc2 (List Feature),
    Mbc3 (List Feature),
    Mbc5 (List Feature),
    Mbc6 (List Feature),
    Mbc7 (List Feature),
    Mmm01 (List Feature),
    Special [PocketCamera, BandaiTama5, HuC3, HuC1 (List Feature)],
    Unknown,
]
    implements [Inspect]

mapType : List U8 -> Type
mapType = \bytes ->
    @Type
        (
            when bytes is
                [0x00] -> Rom []
                [0x01] -> Mbc1 []
                [0x02] -> Mbc1 [Ram]
                [0x03] -> Mbc1 [Ram, Battery]
                [0x05] -> Mbc2 []
                [0x06] -> Mbc2 [Battery]
                [0x08] -> Rom [Ram]
                [0x09] -> Rom [Ram, Battery]
                [0x0B] -> Mmm01 []
                [0x0C] -> Mmm01 [Ram]
                [0x0D] -> Mmm01 [Ram, Battery]
                [0x0F] -> Mbc3 [Timer, Battery]
                [0x10] -> Mbc3 [Timer, Ram, Battery]
                [0x11] -> Mbc3 []
                [0x12] -> Mbc3 [Ram]
                [0x13] -> Mbc3 [Ram, Battery]
                [0x19] -> Mbc5 []
                [0x1A] -> Mbc5 [Ram]
                [0x1B] -> Mbc5 [Ram, Battery]
                [0x1C] -> Mbc5 [Rumble]
                [0x1D] -> Mbc5 [Rumble, Ram]
                [0x1E] -> Mbc5 [Rumble, Ram, Battery]
                [0x20] -> Mbc6 []
                [0x22] -> Mbc7 [Sensor, Rumble, Ram, Battery]
                [0xFC] -> Special PocketCamera
                [0xFD] -> Special BandaiTama5
                [0xFE] -> Special HuC3
                [0xFF] -> Special (HuC1 [Ram, Battery])
                _ -> Unknown
        )

OldLicensee := [None, Some Str, NewLicensee, Unknown] implements [Inspect]

mapOldLicensee : List U8 -> OldLicensee
mapOldLicensee = \bytes ->
    @OldLicensee
        (
            when bytes is
                [0x00] -> None
                [0x01] -> Some "Nintendo"
                [0x08] -> Some "Capcom"
                [0x09] -> Some "HOT-B"
                [0x0A] -> Some "Jaleco"
                [0x0B] -> Some "Coconuts Japan"
                [0x0C] -> Some "Elite Systems"
                [0x13] -> Some "EA (Electronic Arts)"
                [0x18] -> Some "Hudson Soft"
                [0x19] -> Some "ITC Entertainment"
                [0x1A] -> Some "Yanoman"
                [0x1D] -> Some "Japan Clary"
                [0x1F] -> Some "Virgin Games Ltd."
                [0x24] -> Some "PCM Complete"
                [0x25] -> Some "San-X"
                [0x28] -> Some "Kemco"
                [0x29] -> Some "SETA Corporation"
                [0x30] -> Some "Infogrames5"
                [0x31] -> Some "Nintendo"
                [0x32] -> Some "Bandai"
                [0x33] -> NewLicensee
                [0x34] -> Some "Konami"
                [0x35] -> Some "HectorSoft"
                [0x38] -> Some "Capcom"
                [0x39] -> Some "Banpresto"
                [0x3C] -> Some ".Entertainment i"
                [0x3E] -> Some "Gremlin"
                [0x41] -> Some "Ubi Soft"
                [0x42] -> Some "Atlus"
                [0x44] -> Some "Malibu Interactive"
                [0x46] -> Some "Angel"
                [0x47] -> Some "Spectrum Holoby"
                [0x49] -> Some "Irem"
                [0x4A] -> Some "Virgin Games Ltd."
                [0x4D] -> Some "Malibu Interactive"
                [0x4F] -> Some "U.S. Gold"
                [0x50] -> Some "Absolute"
                [0x51] -> Some "Acclaim Entertainment"
                [0x52] -> Some "Activision"
                [0x53] -> Some "Sammy USA Corporation"
                [0x54] -> Some "GameTek"
                [0x55] -> Some "Park Place"
                [0x56] -> Some "LJN"
                [0x57] -> Some "Matchbox"
                [0x59] -> Some "Milton Bradley Company"
                [0x5A] -> Some "Mindscape"
                [0x5B] -> Some "Romstar"
                [0x5C] -> Some "Naxat Soft13"
                [0x5D] -> Some "Tradewest"
                [0x60] -> Some "Titus Interactive"
                [0x61] -> Some "Virgin Games Ltd."
                [0x67] -> Some "Ocean Software"
                [0x69] -> Some "EA (Electronic Arts)"
                [0x6E] -> Some "Elite Systems"
                [0x6F] -> Some "Electro Brain"
                [0x70] -> Some "Infogrames5"
                [0x71] -> Some "Interplay Entertainment"
                [0x72] -> Some "Broderbund"
                [0x73] -> Some "Sculptured Software"
                [0x75] -> Some "The Sales Curve Limited"
                [0x78] -> Some "THQ"
                [0x79] -> Some "Accolade"
                [0x7A] -> Some "Triffix Entertainment"
                [0x7C] -> Some "Microprose"
                [0x7F] -> Some "Kemco"
                [0x80] -> Some "Misawa Entertainment"
                [0x83] -> Some "Lozc"
                [0x86] -> Some "Tokuma Shoten"
                [0x8B] -> Some "Bullet-Proof Software"
                [0x8C] -> Some "Vic Tokai"
                [0x8E] -> Some "Ape"
                [0x8F] -> Some "I’Max"
                [0x91] -> Some "Chunsoft Co.8"
                [0x92] -> Some "Video System"
                [0x93] -> Some "Tsubaraya Productions"
                [0x95] -> Some "Varie"
                [0x96] -> Some "Yonezawa/S’Pal"
                [0x97] -> Some "Kemco"
                [0x99] -> Some "Arc"
                [0x9A] -> Some "Nihon Bussan"
                [0x9B] -> Some "Tecmo"
                [0x9C] -> Some "Imagineer"
                [0x9D] -> Some "Banpresto"
                [0x9F] -> Some "Nova"
                [0xA1] -> Some "Hori Electric"
                [0xA2] -> Some "Bandai"
                [0xA4] -> Some "Konami"
                [0xA6] -> Some "Kawada"
                [0xA7] -> Some "Takara"
                [0xA9] -> Some "Technos Japan"
                [0xAA] -> Some "Broderbund"
                [0xAC] -> Some "Toei Animation"
                [0xAD] -> Some "Toho"
                [0xAF] -> Some "Namco"
                [0xB0] -> Some "Acclaim Entertainment"
                [0xB1] -> Some "ASCII Corporation or Nexsoft"
                [0xB2] -> Some "Bandai"
                [0xB4] -> Some "Square Enix"
                [0xB6] -> Some "HAL Laboratory"
                [0xB7] -> Some "SNK"
                [0xB9] -> Some "Pony Canyon"
                [0xBA] -> Some "Culture Brain"
                [0xBB] -> Some "Sunsoft"
                [0xBD] -> Some "Sony Imagesoft"
                [0xBF] -> Some "Sammy Corporation"
                [0xC0] -> Some "Taito"
                [0xC2] -> Some "Kemco"
                [0xC3] -> Some "Square"
                [0xC4] -> Some "Tokuma Shoten"
                [0xC5] -> Some "Data East"
                [0xC6] -> Some "Tonkinhouse"
                [0xC8] -> Some "Koei"
                [0xC9] -> Some "UFL"
                [0xCA] -> Some "Ultra"
                [0xCB] -> Some "Vap"
                [0xCC] -> Some "Use Corporation"
                [0xCD] -> Some "Meldac"
                [0xCE] -> Some "Pony Canyon"
                [0xCF] -> Some "Angel"
                [0xD0] -> Some "Taito"
                [0xD1] -> Some "Sofel"
                [0xD2] -> Some "Quest"
                [0xD3] -> Some "Sigma Enterprises"
                [0xD4] -> Some "ASK Kodansha Co."
                [0xD6] -> Some "Naxat Soft"
                [0xD7] -> Some "Copya System"
                [0xD9] -> Some "Banpresto"
                [0xDA] -> Some "Tomy"
                [0xDB] -> Some "LJN"
                [0xDD] -> Some "NCS"
                [0xDE] -> Some "Human"
                [0xDF] -> Some "Altron"
                [0xE0] -> Some "Jaleco"
                [0xE1] -> Some "Towa Chiki"
                [0xE2] -> Some "Yutaka"
                [0xE3] -> Some "Varie"
                [0xE5] -> Some "Epcoh"
                [0xE7] -> Some "Athena"
                [0xE8] -> Some "Asmik Ace Entertainment"
                [0xE9] -> Some "Natsume"
                [0xEA] -> Some "King Records"
                [0xEB] -> Some "Atlus"
                [0xEC] -> Some "Epic/Sony Records"
                [0xEE] -> Some "IGS"
                [0xF0] -> Some "A Wave"
                [0xF3] -> Some "Extreme Entertainment"
                [0xFF] -> Some "LJN"
                _ -> Unknown
        )

NewLicensee := [None, Some Str, Unknown] implements [Inspect]

mapNewLicensee : List U8 -> NewLicensee
mapNewLicensee = \bytes ->
    str = Str.fromUtf8 bytes
    @NewLicensee
        (
            when str is
                Ok "00" -> None
                Ok "01" -> Some "Nintendo Research & Development 1"
                Ok "08" -> Some "Capcom"
                Ok "13" -> Some "EA (Electronic Arts)"
                Ok "18" -> Some "Hudson Soft"
                Ok "19" -> Some "B-AI"
                Ok "20" -> Some "KSS"
                Ok "22" -> Some "Planning Office WADA"
                Ok "24" -> Some "PCM Complete"
                Ok "25" -> Some "San-X"
                Ok "28" -> Some "Kemco"
                Ok "29" -> Some "SETA Corporation"
                Ok "30" -> Some "Viacom"
                Ok "31" -> Some "Nintendo"
                Ok "32" -> Some "Bandai"
                Ok "33" -> Some "Ocean Software/Acclaim Entertainment"
                Ok "34" -> Some "Konami"
                Ok "35" -> Some "HectorSoft"
                Ok "37" -> Some "Taito"
                Ok "38" -> Some "Hudson Soft"
                Ok "39" -> Some "Banpresto"
                Ok "41" -> Some "Ubi Soft"
                Ok "42" -> Some "Atlus"
                Ok "44" -> Some "Malibu Interactive"
                Ok "46" -> Some "Angel"
                Ok "47" -> Some "Bullet-Proof Software"
                Ok "49" -> Some "Irem"
                Ok "50" -> Some "Absolute"
                Ok "51" -> Some "Acclaim Entertainment"
                Ok "52" -> Some "Activision"
                Ok "53" -> Some "Sammy USA Corporation"
                Ok "54" -> Some "Konami"
                Ok "55" -> Some "Hi Tech Expressions"
                Ok "56" -> Some "LJN"
                Ok "57" -> Some "Matchbox"
                Ok "58" -> Some "Mattel"
                Ok "59" -> Some "Milton Bradley Company"
                Ok "60" -> Some "Titus Interactive"
                Ok "61" -> Some "Virgin Games Ltd."
                Ok "64" -> Some "Lucasfilm Games4"
                Ok "67" -> Some "Ocean Software"
                Ok "69" -> Some "EA (Electronic Arts)"
                Ok "70" -> Some "Infogrames"
                Ok "71" -> Some "Interplay Entertainment"
                Ok "72" -> Some "Broderbund"
                Ok "73" -> Some "Sculptured Software"
                Ok "75" -> Some "The Sales Curve Limited"
                Ok "78" -> Some "THQ"
                Ok "79" -> Some "Accolade"
                Ok "80" -> Some "Misawa Entertainment"
                Ok "83" -> Some "lozc"
                Ok "86" -> Some "Tokuma Shoten"
                Ok "87" -> Some "Tsukuda Original"
                Ok "91" -> Some "Chunsoft Co."
                Ok "92" -> Some "Video System"
                Ok "93" -> Some "Ocean Software/Acclaim Entertainment"
                Ok "95" -> Some "Varie"
                Ok "96" -> Some "Yonezawa/S’pal"
                Ok "97" -> Some "Kaneko"
                Ok "99" -> Some "Pack-In-Video"
                Ok "9H" -> Some "Bottom Up"
                Ok "A4" -> Some "Konami (Yu-Gi-Oh!)"
                Ok "BL" -> Some "MTO"
                Ok "DK" -> Some "Kodansha"
                _ -> Unknown
        )

Size := [KiB U16, MiB U16, Unknown] implements [Inspect]

mapRamSize : List U8 -> Size
mapRamSize = \bytes ->
    @Size
        (
            when bytes is
                [0x00] -> KiB 0
                [0x02] -> KiB 8
                [0x03] -> KiB 32
                [0x04] -> KiB 128
                [0x05] -> KiB 64
                _ -> Unknown
        )

mapRomSize : List U8 -> Size
mapRomSize = \bytes ->
    @Size
        (
            when bytes is
                [0x00] -> KiB 32
                [0x01] -> KiB 64
                [0x02] -> KiB 128
                [0x03] -> KiB 256
                [0x04] -> KiB 512
                [0x05] -> MiB 1
                [0x06] -> MiB 2
                [0x07] -> MiB 4
                [0x08] -> MiB 8
                _ -> Unknown
        )

actualChecksum : List U8 -> U8
actualChecksum = \data ->
    data
    |> List.sublist { start: 0x0134, len: 25 }
    |> List.walk 0x00 (\state, elem -> (state |> Num.subWrap elem |> Num.subWrap 1))

# mapChecksum : U8, List U8 -> Result [ValidChecksum U8] [InvalidChecksum U8, OutOfBounds]
mapChecksum = \value, data ->
    exp = (List.get value 0)?
    actual = actualChecksum data
    when Num.compare exp actual is
        EQ -> Ok (ValidChecksum actual)
        _ -> Err (InvalidChecksum actual)

mapDestination = \bytes ->
    when bytes is
        [0x00] -> Some "Japan (and possibly overseas)"
        [0x01] -> Some "Overseas only"
        _ -> Unknown

# In older cartridges this byte was part of the Title (see above). The CGB and later models interpret this byte to decide whether to enable Color mode (“CGB Mode”) or to fall back to monochrome compatibility mode (“Non-CGB Mode”).
mapCgbFlag : List U8 -> Bool
mapCgbFlag = \bytes ->
    when bytes is
        [0x80] -> Bool.true # The game supports CGB enhancements, but is backwards compatible with monochrome Game Boys
        [0xC0] -> Bool.true # The game works on CGB only (the hardware ignores bit 6, so this really functions the same as $80)
        _ -> Bool.false

# This byte specifies whether the game supports SGB functions. The SGB will ignore any command packets if this byte is set to a value other than $03 (typically $00).
mapSgbFlag : List U8 -> Bool
mapSgbFlag = \bytes ->
    when bytes is
        [0x03] -> Bool.true
        _ -> Bool.false

Props : [
    Entry,
    Logo,
    Title,
    Manufacturer,
    CgbFlag,
    NewLicensee,
    SgbFlag,
    Type,
    RamSize,
    RomSize,
    Destination,
    OldLicensee,
    Version,
    Checksum,
]

getHeaderProp : List U8, Props -> List U8
getHeaderProp = \data, prop ->
    data
    |> List.sublist
        (
            when prop is
                Entry -> { start: 0x0100, len: 4 }
                Logo -> { start: 0x0104, len: 30 }
                Title -> { start: 0x0134, len: 16 }
                Manufacturer -> { start: 0x013F, len: 4 }
                CgbFlag -> { start: 0x0143, len: 1 }
                NewLicensee -> { start: 0x0144, len: 2 }
                SgbFlag -> { start: 0x0146, len: 1 }
                Type -> { start: 0x0147, len: 1 }
                RomSize -> { start: 0x0148, len: 1 }
                RamSize -> { start: 0x0149, len: 1 }
                Destination -> { start: 0x014A, len: 1 }
                OldLicensee -> { start: 0x014B, len: 1 }
                Version -> { start: 0x014C, len: 1 }
                Checksum -> { start: 0x014D, len: 1 }
        )

read = \data -> {
    entry: getHeaderProp data Entry,
    logo: getHeaderProp data Logo,
    title: getHeaderProp data Title |> Str.fromUtf8,
    manufacturer: getHeaderProp data Manufacturer |> Str.fromUtf8,
    cgbFlag: getHeaderProp data CgbFlag |> mapCgbFlag,
    sgbFlag: getHeaderProp data SgbFlag |> mapSgbFlag,
    type: getHeaderProp data Type |> mapType,
    romSize: getHeaderProp data RomSize |> mapRomSize,
    ramSize: getHeaderProp data RamSize |> mapRamSize,
    oldLicensee: getHeaderProp data OldLicensee |> mapOldLicensee,
    newlicensee: getHeaderProp data NewLicensee |> mapNewLicensee,
    version: getHeaderProp data Version,
    destination: getHeaderProp data Destination |> mapDestination,
    checksum: getHeaderProp data Checksum |> mapChecksum data,
}

use enumset::enum_set;
use std::collections::HashMap;
use std::{env, fs::File, io::{self, BufRead}, process::exit};

use enumset::{EnumSet, EnumSetType};

#[derive(Debug, PartialEq, Eq, Hash)]
enum Sound {
    AA,
    AE,
    AH,
    AO,
    AW,
    AY,
    B,
    CH,
    D,
    DH,
    EH,
    ER,
    EY,
    F,
    G,
    HH,
    IH,
    IY,
    JH,
    K,
    L,
    M,
    N,
    NG,
    OW,
    OY,
    P,
    R,
    S,
    SH,
    T,
    TH,
    UH,
    UW,
    V,
    W,
    Y,
    Z,
    ZH,
}

impl Sound {
    fn from_str(s: &str) -> Option<Sound> {
        use Sound::*;
        match s {
            "AA" => Some(AA),
            "AA0" => Some(AA),
            "AA1" => Some(AA),
            "AA2" => Some(AA),
            "AE" => Some(AE),
            "AE0" => Some(AE),
            "AE1" => Some(AE),
            "AE2" => Some(AE),
            "AH" => Some(AH),
            "AH0" => Some(AH),
            "AH1" => Some(AH),
            "AH2" => Some(AH),
            "AO" => Some(AO),
            "AO0" => Some(AO),
            "AO1" => Some(AO),
            "AO2" => Some(AO),
            "AW" => Some(AW),
            "AW0" => Some(AW),
            "AW1" => Some(AW),
            "AW2" => Some(AW),
            "AY" => Some(AY),
            "AY0" => Some(AY),
            "AY1" => Some(AY),
            "AY2" => Some(AY),
            "B" => Some(B),
            "CH" => Some(CH),
            "D" => Some(D),
            "DH" => Some(DH),
            "EH" => Some(EH),
            "EH0" => Some(EH),
            "EH1" => Some(EH),
            "EH2" => Some(EH),
            "ER" => Some(ER),
            "ER0" => Some(ER),
            "ER1" => Some(ER),
            "ER2" => Some(ER),
            "EY" => Some(EY),
            "EY0" => Some(EY),
            "EY1" => Some(EY),
            "EY2" => Some(EY),
            "F" => Some(F),
            "G" => Some(G),
            "HH" => Some(HH),
            "IH" => Some(IH),
            "IH0" => Some(IH),
            "IH1" => Some(IH),
            "IH2" => Some(IH),
            "IY" => Some(IY),
            "IY0" => Some(IY),
            "IY1" => Some(IY),
            "IY2" => Some(IY),
            "JH" => Some(JH),
            "K" => Some(K),
            "L" => Some(L),
            "M" => Some(M),
            "N" => Some(N),
            "NG" => Some(NG),
            "OW" => Some(OW),
            "OW0" => Some(OW),
            "OW1" => Some(OW),
            "OW2" => Some(OW),
            "OY" => Some(OY),
            "OY0" => Some(OY),
            "OY1" => Some(OY),
            "OY2" => Some(OY),
            "P" => Some(P),
            "R" => Some(R),
            "S" => Some(S),
            "SH" => Some(SH),
            "T" => Some(T),
            "TH" => Some(TH),
            "UH" => Some(UH),
            "UH0" => Some(UH),
            "UH1" => Some(UH),
            "UH2" => Some(UH),
            "UW" => Some(UW),
            "UW0" => Some(UW),
            "UW1" => Some(UW),
            "UW2" => Some(UW),
            "V" => Some(V),
            "W" => Some(W),
            "Y" => Some(Y),
            "Z" => Some(Z),
            "ZH" => Some(ZH),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct WordEntry {
    word: String,
    sounds: Vec<Sound>,
}

impl WordEntry {
    fn from_line(line: &str) -> Result<WordEntry, String> {
        let mut iter = line.split_ascii_whitespace();
        if let Some(word) = iter.next() {
            return Result::Ok(WordEntry {
                word: word.to_string(),
                sounds: iter.map(|x| Sound::from_str(x).expect("Valid sound")).collect(),
            });
        }
        Result::Err("Empty line".to_owned())
    }
}

#[derive(Debug, EnumSetType, PartialOrd, Ord)]
#[enumset(repr = "u32")]
enum StenoKey {
    LS, LT, LK, LP, LW, LH, LR, A, O, Star, E, U, RF, RR, RP, RB, RL, RG, RT, RS, RD, RZ,
}

fn remove_compound_sound(keys: &mut EnumSet<StenoKey>, compound: EnumSet<StenoKey>) -> bool {
    let compound_repr = compound.as_repr();
    let lesser_bits = (1 << compound_repr.trailing_zeros()) - 1;
    let middle_bits = (1 << (compound_repr.ilog2() + 1)) - 1;
    let mask = lesser_bits ^ middle_bits;
    if compound_repr == (keys.as_repr() & mask) {
        keys.remove_all(compound);
        return true;
    }
    false

}

fn parse_steno_chord(chord: &str) -> Option<EnumSet<StenoKey>> {
    let mut chars = chord.chars().rev().peekable();
    fn pop_shift<T : Iterator<Item = char>>(c: char, chars: &mut std::iter::Peekable<T>, set: &mut u32) {
        *set = *set << 1;
        if chars.next_if_eq(&c).is_some() {
            *set = *set | 1;
        }
    }
    let mut set = EnumSet::<StenoKey>::empty().as_repr();
    pop_shift('Z', &mut chars, &mut set);
    pop_shift('D', &mut chars, &mut set);
    pop_shift('S', &mut chars, &mut set);
    pop_shift('T', &mut chars, &mut set);
    pop_shift('G', &mut chars, &mut set);
    pop_shift('L', &mut chars, &mut set);
    pop_shift('B', &mut chars, &mut set);
    pop_shift('P', &mut chars, &mut set);
    pop_shift('R', &mut chars, &mut set);
    pop_shift('F', &mut chars, &mut set);
    chars.next_if_eq(&'-');
    pop_shift('U', &mut chars, &mut set);
    pop_shift('E', &mut chars, &mut set);
    pop_shift('*', &mut chars, &mut set);
    pop_shift('O', &mut chars, &mut set);
    pop_shift('A', &mut chars, &mut set);
    pop_shift('R', &mut chars, &mut set);
    pop_shift('H', &mut chars, &mut set);
    pop_shift('W', &mut chars, &mut set);
    pop_shift('P', &mut chars, &mut set);
    pop_shift('K', &mut chars, &mut set);
    pop_shift('T', &mut chars, &mut set);
    pop_shift('S', &mut chars, &mut set);
    if chars.peek().is_some() {
        None
    } else {
        Some(EnumSet::from_repr(set))
    }
}

fn steno_keys_to_sounds(keys: &EnumSet<StenoKey>) -> Vec<Sound> {
    use StenoKey::*;
    // NOTE(vipa, 2025-02-04): Assumption: "compound sounds" will only
    // be used when there's no other key pressed between the first and
    // last sound that's part of that compound. This means we work
    // through the compounds in order of where they start.

    let mut keys = keys.clone();
    let mut ret = vec![];

    // NOTE(vipa, 2025-02-04): LS
    if remove_compound_sound(&mut keys, LS | Star) {
        ret.push(Sound::Z);
    }
    if remove_compound_sound(&mut keys, LS | LR) {
        ret.push(Sound::V);
    }
    if remove_compound_sound(&mut keys, LS | LK | LW | LR) {
        ret.push(Sound::JH);
    }
    if remove_compound_sound(&mut keys, LS | LH) {
        ret.push(Sound::SH);
    }
    if keys.contains(LS) {
        ret.push(Sound::S);
    }

    // NOTE(vipa, 2025-02-04): LT
    if remove_compound_sound(&mut keys, LT | LH) {
        ret.push(Sound::TH);
    }
    if remove_compound_sound(&mut keys, LT | LP | LH) {
        ret.push(Sound::N);
    }
    if remove_compound_sound(&mut keys, LT | LH) {
        ret.push(Sound::TH);
    }
    if remove_compound_sound(&mut keys, LT | LK | LP | LW) {
        ret.push(Sound::G);
    }
    if remove_compound_sound(&mut keys, LT | LP) {
        ret.push(Sound::F);
    }
    if remove_compound_sound(&mut keys, LT | LK) {
        ret.push(Sound::D);
    }
    if keys.contains(LT) {
        ret.push(Sound::T);
    }

    // NOTE(vipa, 2025-02-04): LK
    if remove_compound_sound(&mut keys, LK | LW | LR) {
        ret.push(Sound::Y);
    }
    if remove_compound_sound(&mut keys, LK | LH) {
        ret.push(Sound::CH);
    }
    if keys.contains(LK) {
        ret.push(Sound::K);
    }

    // NOTE(vipa, 2025-02-04): LP
    if remove_compound_sound(&mut keys, LP | LH) {
        ret.push(Sound::M);
    }
    if remove_compound_sound(&mut keys, LP | LW) {
        ret.push(Sound::B);
    }
    if keys.contains(LP) {
        ret.push(Sound::P);
    }

    // NOTE(vipa, 2025-02-04): LW
    if keys.contains(LW) {
        ret.push(Sound::W);
    }

    // NOTE(vipa, 2025-02-04): LH
    if remove_compound_sound(&mut keys, LH | LR) {
        ret.push(Sound::L);
    }
    if keys.contains(LH) {
        ret.push(Sound::HH);
    }

    // NOTE(vipa, 2025-02-04): LR
    if keys.contains(LR) {
        ret.push(Sound::R);
    }

    // NOTE(vipa, 2025-02-04): Vowels
    let s = match (keys.contains(A), keys.contains(O), keys.contains(E), keys.contains(U)) {
        (false, false, true, true) => Some(Sound::IH), // short i
        (false, true, false, true) => Some(Sound::OW), // ow, (boat)
        (false, true, true, false) => Some(Sound::AO), // o with a hat, (door)
        (false, true, true, true) => Some(Sound::OY), // oi, (boy)
        (true, false, false, true) => Some(Sound::AW), // aw,
        (true, false, true, false) => Some(Sound::AE), // spelling?
        (true, false, true, true) => Some(Sound::EY), // a with a hat, (clay)
        (true, true, false, false) => Some(Sound::UW), // "oo" "oa", (dune)
        (true, true, false, true) => Some(Sound::UW), // ew, (slew)
        (true, true, true, false) => Some(Sound::IY), // e with a hat, (feel)
        (true, true, true, true) => Some(Sound::AY), // long i

        (true, false, false, false) => Some(Sound::AH),
        (false, true, false, false) => Some(Sound::AA),
        (false, false, true, false) => Some(Sound::EH),
        (false, false, false, true) => Some(Sound::UW),

        (false, false, false, false) => None,
    };
    if let Some(s) = s {
        ret.push(s);
    }

    // NOTE(vipa, 2025-02-04): Star
    if remove_compound_sound(&mut keys, Star | RT) {
        ret.push(Sound::TH);
    }
    if remove_compound_sound(&mut keys, Star | RL | RG) {
        ret.push(Sound::L);
        ret.push(Sound::K);
    }
    if remove_compound_sound(&mut keys, Star | RP | RB | RG) {
        ret.push(Sound::NG);
        ret.push(Sound::K);
    }
    if remove_compound_sound(&mut keys, Star | RP | RL) {
        ret.push(Sound::M);
        ret.push(Sound::P);
    }

    // NOTE(vipa, 2025-02-04): RF
    if remove_compound_sound(&mut keys, RF | RR | RB) {
        ret.push(Sound::R);
        ret.push(Sound::V);
    }
    if remove_compound_sound(&mut keys, RF | RR | RP | RB) {
        ret.push(Sound::R); // NOTE(vipa, 2025-02-04): Should also be NCH
        ret.push(Sound::CH);
    }
    if remove_compound_sound(&mut keys, RF | RP) {
        ret.push(Sound::CH);
    }
    if keys.contains(RF) {
        ret.push(Sound::F);
    }

    // NOTE(vipa, 2025-02-04): RR
    if remove_compound_sound(&mut keys, RR | RB) {
        ret.push(Sound::SH);
    }
    if keys.contains(RR) {
        ret.push(Sound::R);
    }

    // NOTE(vipa, 2025-02-04): RP
    if remove_compound_sound(&mut keys, RP | RB | RL | RG) {
        ret.push(Sound::JH);
    }
    if remove_compound_sound(&mut keys, RP | RL) {
        ret.push(Sound::M);
    }
    if remove_compound_sound(&mut keys, RP | RB) {
        ret.push(Sound::N);
    }
    if keys.contains(RP) {
        ret.push(Sound::P);
    }

    // NOTE(vipa, 2025-02-04): RB
    if remove_compound_sound(&mut keys, RB | RG | RS) {
        ret.push(Sound::K);
        ret.push(Sound::SH);
        ret.push(Sound::AH);
        ret.push(Sound::N);
    }
    if remove_compound_sound(&mut keys, RB | RG) {
        ret.push(Sound::K);
    }
    if keys.contains(RB) {
        ret.push(Sound::B);
    }

    // NOTE(vipa, 2025-02-04): RL
    if remove_compound_sound(&mut keys, RL | RG) {
        ret.push(Sound::L);
        ret.push(Sound::CH);
    }
    if keys.contains(RL) {
        ret.push(Sound::L);
    }

    // NOTE(vipa, 2025-02-04): RG
    if remove_compound_sound(&mut keys, RG | RS) {
        ret.push(Sound::SH);
        ret.push(Sound::AH);
        ret.push(Sound::N);
    }
    if keys.contains(RG) {
        ret.push(Sound::G);
    }

    if keys.contains(RT) {
        ret.push(Sound::T);
    }
    if keys.contains(RS) {
        ret.push(Sound::S);
    }
    if keys.contains(RD) {
        ret.push(Sound::D);
    }
    if keys.contains(RZ) {
        ret.push(Sound::Z);
    }

    ret
}

fn main() {
    match &env::args().collect::<Vec<_>>().as_slice() {
        &[_, dict, ref words @ ..] => {
            let file = File::open(dict).expect("Missing file probably");
            let entries = io::BufReader::new(file)
                .lines()
                .filter_map(|x| x.ok())
                .filter(|x| !x.starts_with(";;;"))
                .map(|x| WordEntry::from_line(&x).expect("Correct word entry"));
            let chords : Vec<_> = words.iter()
                .map(|x| parse_steno_chord(x).expect("Correct chord"))
                .collect();
            let sounds : Vec<_> = chords.iter()
                .flat_map(|x| steno_keys_to_sounds(&x).into_iter())
                .collect();
            let matches : Vec<_> = entries
                .filter(|x| x.sounds == sounds)
                .map(|x| x.word)
                .collect();

            println!("{:?} {:?} {:?}", chords, sounds, matches)
        }
        _ => {
            println!("Need at least cmudict as an argument");
            exit(1);
        }
    }
}

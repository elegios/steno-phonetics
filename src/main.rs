use bitvec::{order::Msb0, view::BitView};
use color_eyre::{
    eyre::{eyre, Context, OptionExt},
    Result,
};
use enumset::{enum_set, EnumSet, EnumSetType};
use lru::LruCache;
use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    env,
    fmt::{self, Debug},
    fs::File,
    io::{self, BufRead},
    iter,
    num::NonZero,
};
use tokio::{
    io::AsyncReadExt,
    sync::mpsc::{self, Receiver, Sender},
};
use tokio_serial::SerialPortBuilderExt;
use trie_rs::{
    inc_search::IncSearch,
    map::{Trie, TrieBuilder},
};

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord, Clone, Copy)]
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
    // DH, // NOTE(vipa, 2025-02-09): We make no distinction between DH and TH
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
    // ZH, // NOTE(vipa, 2025-02-09): We make no distinction between ZH and SH
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
            "DH" => Some(TH),
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
            "ZH" => Some(SH),
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
    fn from_line(line: &str) -> Result<WordEntry> {
        let mut iter = line.split_ascii_whitespace();
        if let Some(word) = iter.next() {
            let word = word.trim_end_matches("(1)").trim_end_matches("(2)");
            let sounds: Result<Vec<_>> = iter
                .map(|x| Sound::from_str(x).ok_or_eyre("Invalid sound"))
                .collect();
            return Ok(WordEntry {
                word: word.to_string(),
                sounds: sounds?,
            });
        }
        Err(eyre!("Empty line"))
    }
}

#[derive(Debug, EnumSetType, PartialOrd, Ord)]
#[enumset(repr = "u32")]
enum StenoKey {
    LS,
    LT,
    LK,
    LP,
    LW,
    LH,
    LR,
    A,
    O,
    Star,
    E,
    U,
    RF,
    RR,
    RP,
    RB,
    RL,
    RG,
    RT,
    RS,
    RD,
    RZ,
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

fn parse_steno_chord(chord: &str) -> Result<EnumSet<StenoKey>> {
    let mut chars = chord.chars().rev().peekable();
    fn pop_shift<T: Iterator<Item = char>>(
        c: char,
        chars: &mut std::iter::Peekable<T>,
        set: &mut u32,
    ) {
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
        let remaining: Vec<_> = chars.collect();
        Err(eyre!(
            "Not all characters where consumed (remaining: {:?})",
            remaining
        ))
    } else {
        Ok(EnumSet::from_repr(set))
    }
}

fn bytes_to_steno_chord(bytes: &[u8; BYTES_PER_STROKE]) -> Result<EnumSet<StenoKey>> {
    use StenoKey::*;
    const KEYS: [[EnumSet<StenoKey>; 7]; BYTES_PER_STROKE] = [
        [
            enum_set!(),
            enum_set!(), /* left number */
            enum_set!(), /* right number */
            enum_set!(),
            enum_set!(),
            enum_set!(),
            enum_set!(),
        ],
        [
            enum_set!(LS),
            enum_set!(LS),
            enum_set!(LT),
            enum_set!(LK),
            enum_set!(LP),
            enum_set!(LW),
            enum_set!(LH),
        ],
        [
            enum_set!(LR),
            enum_set!(A),
            enum_set!(O),
            enum_set!(Star),
            enum_set!(Star),
            enum_set!(),
            enum_set!(),
        ],
        [
            enum_set!(),
            enum_set!(Star),
            enum_set!(Star),
            enum_set!(E),
            enum_set!(U),
            enum_set!(RF),
            enum_set!(RR),
        ],
        [
            enum_set!(RP),
            enum_set!(RB),
            enum_set!(RL),
            enum_set!(RG),
            enum_set!(RT),
            enum_set!(RS),
            enum_set!(RD),
        ],
        [
            enum_set!(),
            enum_set!(),
            enum_set!(),
            enum_set!(),
            enum_set!(),
            enum_set!(),
            enum_set!(RZ),
        ],
    ];

    if !bytes[0].view_bits::<Msb0>()[0] {
        return Err(eyre!("Bad first bit in steno chord: {:?}", bytes));
    }
    for b in &bytes[1..] {
        if b.view_bits::<Msb0>()[0] {
            return Err(eyre!("Bad follow bit in steno chord: {:?}", bytes));
        }
    }
    Ok(bytes
        .iter()
        .zip(KEYS.iter())
        .flat_map(|(bits, keys)| {
            bits.view_bits::<Msb0>()[1..]
                .iter()
                .by_vals()
                .zip(keys.iter())
        })
        .filter(|(bit, _)| *bit)
        .map(|(_, x)| *x)
        .collect())
}

fn steno_keys_to_sounds(keys: EnumSet<StenoKey>) -> Vec<Sound> {
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
    if !keys.contains(LR) && remove_compound_sound(&mut keys, LT | LP | LH) {
        // NOTE(vipa, 2025-02-09): We prioritize F L over N R
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
    if !keys.contains(LR) && remove_compound_sound(&mut keys, LP | LH) {
        // NOTE(vipa, 2025-02-15): We prioritize P L over M R
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

    // NOTE(vipa, 2025-02-07): Added, may or may not be a good idea
    if remove_compound_sound(&mut keys, A | RR) {
        ret.push(Sound::ER);
    } else if remove_compound_sound(&mut keys, O | RR) {
        ret.push(Sound::ER);
    } else if remove_compound_sound(&mut keys, E | RR) {
        ret.push(Sound::ER);
    } else if remove_compound_sound(&mut keys, U | RR) {
        ret.push(Sound::ER);
    }

    // NOTE(vipa, 2025-02-04): Vowels
    let s = match (
        keys.contains(A),
        keys.contains(O),
        keys.contains(E),
        keys.contains(U),
    ) {
        (false, false, true, true) => Some(Sound::IH), // short i
        (false, true, false, true) => Some(Sound::OW), // ow, (boat)
        (false, true, true, false) => Some(Sound::AO), // o with a hat, (door)
        (false, true, true, true) => Some(Sound::OY),  // oi, (boy)
        (true, false, false, true) => Some(Sound::AW), // aw,
        (true, false, true, false) => Some(Sound::AE), // spelling?
        (true, false, true, true) => Some(Sound::EY),  // a with a hat, (clay)
        (true, true, false, false) => Some(Sound::UW), // "oo" "oa", (dune)
        (true, true, false, true) => Some(Sound::UW),  // ew, (slew)
        (true, true, true, false) => Some(Sound::IY),  // e with a hat, (feel)
        (true, true, true, true) => Some(Sound::AY),   // long i

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
        ret.push(Sound::R); // TODO(vipa, 2025-02-04): Should also be NCH
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

type StateIdx = u32;
type WordIdx = u16;

struct UnfinishedState<'a> {
    prev_finished: Option<StateIdx>,
    search: IncSearch<'a, Sound, usize>,
}

#[derive(Debug)]
struct FinishedState<'a> {
    word: &'a str,
    prev: Option<(StateIdx, WordIdx)>,
    weight: f64,
}

#[derive(Debug)]
struct State<'a> {
    // NOTE(vipa, 2025-02-12): We store all intermediate states
    // between each chord
    unfinished: Vec<Vec<UnfinishedState<'a>>>,
    // NOTE(vipa, 2025-02-12): We only store the most likely path for
    // each final word between each chord
    finished: Vec<Vec<FinishedState<'a>>>,
}

impl<'a> fmt::Debug for UnfinishedState<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "UnfinishedState {{prev_finished: {:?}, search: {:?}}}",
            self.prev_finished,
            self.search.prefix_len()
        )
    }
}

impl<'a> State<'a> {
    fn new(trie: &'a Trie<Sound, usize>) -> Self {
        Self {
            unfinished: vec![vec![UnfinishedState {
                prev_finished: None,
                search: trie.inc_search(),
            }]],
            finished: vec![vec![]],
        }
    }

    fn print_interactive_state(&self) -> Result<()> {
        let (idx, sentence) = (0..=(self.finished.len() - 1))
            .rev()
            .find_map(|idx| self.likeliest_sentence_at(idx).map(|x| (idx, x)))
            .unwrap_or((0, vec![]));

        for w in sentence {
            print!("{} ", w);
        }

        let extra_chords = self.finished.len() - 1 - idx;
        if extra_chords > 0 {
            print!("+ {}", extra_chords);
            let first_dead_idx = (0..=(self.finished.len() - 1))
                .rev()
                .take_while(|idx| {
                    self.finished[*idx].is_empty() && self.unfinished[*idx].is_empty()
                })
                .last();
            if first_dead_idx.is_some() {
                print!(" (dead)")
            }
        }

        println!("");

        Ok(())
    }

    fn likeliest_sentence_at(&self, idx: usize) -> Option<Vec<&'a str>> {
        let mut result = vec![];
        let mut thing = self
            .finished
            .get(idx)
            .iter()
            .flat_map(|x| x.iter())
            .max_by(|l, r| l.weight.total_cmp(&r.weight));

        if thing.is_none() {
            return None;
        }

        while let Some(t) = thing {
            result.push(t.word);
            thing = t
                .prev
                .map(|(si, wi)| &self.finished[si as usize][wi as usize]);
        }
        result.reverse();

        Some(result)
    }

    fn likeliest_sentence(&self) -> Option<Vec<&'a str>> {
        self.likeliest_sentence_at(self.finished.len() - 1)
    }

    const ABSENT_WEIGHT: f64 = -140.0; // NOTE(vipa, 2025-02-12): In log2 space

    fn iter_prev_words_state<'x>(
        prev: Option<StateIdx>,
        finished: &'x Vec<Vec<FinishedState<'a>>>,
    ) -> impl Iterator<Item = (&'a str, Option<(StateIdx, WordIdx)>, f64)> + 'x {
        if let Some(state_idx) = prev {
            either::Left(finished[state_idx as usize].iter().enumerate().map(
                move |(word_idx, x)| (x.word, Some((state_idx, word_idx as WordIdx)), x.weight),
            ))
        } else {
            either::Right(iter::once(("_START_", None, 0.0)))
        }
        .into_iter()
    }

    fn iter_prev_words<'x>(
        prevs: Vec<Option<StateIdx>>,
        finished: &'x Vec<Vec<FinishedState<'a>>>,
    ) -> impl Iterator<Item = (&'a str, Option<(StateIdx, WordIdx)>, f64)> + 'x {
        prevs
            .into_iter()
            .flat_map(|p| Self::iter_prev_words_state(p, finished))
    }

    fn pop_last(&mut self) {
        if self.unfinished.len() > 1 {
            self.unfinished.pop();
            self.finished.pop();
        }
    }

    async fn step(
        &mut self,
        trie: &'a Trie<Sound, usize>,
        words: &'a Vec<Vec<String>>,
        chord: &[Sound],
        weights: &mut LruCache<(&'a str, &'a str), f64>,
        client: &reqwest::Client,
    ) -> Result<()> {
        let mut next_unfinished = vec![];
        let mut to_finish: HashMap<&'a str, Vec<Option<StateIdx>>> = HashMap::new();

        let to_process = self
            .unfinished
            .last()
            .ok_or_eyre("unfinished is never empty")?
            .iter();
        let extra = if self
            .finished
            .last()
            .ok_or_eyre("finished is never empty")?
            .is_empty()
        {
            None
        } else {
            Some(UnfinishedState {
                prev_finished: Some(
                    (self.finished.len() - 1)
                        .try_into()
                        .context("Last index in self.finished should fit in StateIdx")?,
                ),
                search: trie.inc_search(),
            })
        };
        let to_process = extra.iter().chain(to_process);

        for UnfinishedState {
            prev_finished,
            search,
        } in to_process
        {
            let mut search = search.clone();
            if let Ok(answer) = search.query_until(chord) {
                if answer.is_match() {
                    words[*search
                        .value()
                        .ok_or_eyre("answer.is_match is true, thus value should be present")?]
                    .iter()
                    .for_each(|w| to_finish.entry(w).or_default().push(*prev_finished));
                }
                if answer.is_prefix() {
                    next_unfinished.push(UnfinishedState {
                        prev_finished: *prev_finished,
                        search,
                    })
                }
            }
        }

        let mut bigrams: HashSet<(&str, &str)> = HashSet::new();
        to_finish
            .iter()
            .flat_map(|(second_word, prevs)| prevs.iter().zip(iter::repeat(*second_word)))
            .for_each(|(prev, second_word)| {
                if let Some(prev) = prev {
                    bigrams.extend(
                        self.finished[*prev as usize]
                            .iter()
                            .map(|x| x.word)
                            .zip(iter::repeat(second_word)),
                    );
                } else {
                    bigrams.insert(("_START_", second_word));
                }
            });
        bigrams.retain(|x| !weights.contains(x));

        fetch_weights(client, weights, bigrams).await?;

        let mut next_finished = vec![];
        for (word, prevs) in to_finish.into_iter() {
            let (prev, weight) = Self::iter_prev_words(prevs, &self.finished)
                .map(|x| {
                    (
                        x.1,
                        x.2 + weights.get(&(x.0, word)).unwrap_or(&Self::ABSENT_WEIGHT),
                    )
                })
                .max_by(|l: _, r: _| l.1.total_cmp(&r.1))
                .ok_or_eyre("Iterator should be non-empty")?;
            next_finished.push(FinishedState { word, weight, prev });
        }

        self.unfinished.push(next_unfinished);
        self.finished.push(next_finished);

        Ok(())
    }
}

#[derive(Serialize)]
struct NGramRequest {
    queries: Vec<String>,
}

#[derive(Deserialize, Debug)]
#[repr(transparent)]
struct NGramToken {
    text: String,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct NGram {
    // id: String,
    // abs_total_match_count : i64,
    rel_total_match_count: f64,
    // tokens: Vec<NGramToken>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct NGramResult {
    // query_tokens: Vec<NGramToken>,
    ngrams: Option<Vec<NGram>>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct NGramResponse {
    results: Vec<NGramResult>,
}

async fn fetch_weights<'a>(
    client: &reqwest::Client,
    weights: &mut LruCache<(&'a str, &'a str), f64>,
    bigrams: HashSet<(&'a str, &'a str)>,
) -> Result<()> {
    let original_bigrams: Vec<_> = bigrams.into_iter().collect();
    let bigrams = original_bigrams
        .iter()
        .map(|(w1, w2)| format!("{} {}", w1, w2))
        .collect();

    let result: NGramResponse = client
        .post("https://api.ngrams.dev/eng/batch?flags=cr")
        .json(&NGramRequest { queries: bigrams })
        .send()
        .await
        .context("NGram request")?
        .json()
        .await
        .context("NGram json parse")?;

    let iter = original_bigrams.into_iter().zip(result.results.into_iter());
    for (pair, res) in iter {
        if let Some(res) = res.ngrams.as_ref().and_then(|x| x.first()) {
            weights.push(pair, res.rel_total_match_count.log2());
        }
    }
    Ok(())
}

fn parse_dict_file(path: &str) -> Result<(Trie<Sound, usize>, Vec<Vec<String>>)> {
    let file = File::open(path).context("Dictionary file")?;
    let entries = io::BufReader::new(file)
        .lines()
        .filter_map(|x| x.ok())
        .filter(|x| !x.starts_with(";;;"))
        .map(|x| WordEntry::from_line(&x));

    let mut words_by_sounds: BTreeMap<_, Vec<_>> = BTreeMap::new();
    for e in entries {
        let e = e?;
        words_by_sounds.entry(e.sounds).or_default().push(e.word);
    }

    let mut entries = TrieBuilder::new();
    let mut words = vec![];
    words.reserve_exact(words_by_sounds.len());
    for (idx, (sound_seq, w)) in words_by_sounds.into_iter().enumerate() {
        entries.push(sound_seq, idx);
        words.push(w);
    }
    let entries = entries.build();
    Ok((entries, words))
}

async fn run<'a>(
    trie: Trie<Sound, usize>,
    words: Vec<Vec<String>>,
    mut tx: Receiver<EnumSet<StenoKey>>, // mut terminal: DefaultTerminal,
) -> Result<Vec<String>> {
    let mut state = State::new(&trie);
    let client = reqwest::Client::new();
    let mut weights = LruCache::new(NonZero::new(10000).unwrap());

    loop {
        state.print_interactive_state()?;
        if let Some(chord) = tx.recv().await {
            if chord == StenoKey::Star {
                state.pop_last();
            } else {
                let chord = steno_keys_to_sounds(chord);
                state
                    .step(&trie, &words, &chord, &mut weights, &client)
                    .await?
            }
        } else {
            return state
                .likeliest_sentence()
                .map(|x| x.iter().map(|x| x.to_string()).collect())
                .ok_or_eyre("No valid sentence");
        }
    }
}

const BYTES_PER_STROKE: usize = 6;

async fn read_steno_machine<T>(path: &str, tx: Sender<EnumSet<StenoKey>>) -> Result<T> {
    let mut port = tokio_serial::new(path, 9600).open_native_async()?;
    let mut buffer = [0u8; BYTES_PER_STROKE];

    loop {
        port.read_exact(&mut buffer).await?;
        let res = bytes_to_steno_chord(&buffer)?;
        tx.send(res).await?;
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;
    let dict_path = env::args()
        .take(2)
        .last()
        .ok_or_eyre("Need one argument: the path to the phonetic dictionary")?;

    let (trie, words) = parse_dict_file(&dict_path)?;

    // let terminal = ratatui::init();
    // let res = run(&trie, &words, terminal);
    // ratatui::restore();
    // res

    let (send, recv) = mpsc::channel(5);

    let mut set = tokio::task::JoinSet::new();
    set.spawn(read_steno_machine("/dev/ttyACM0", send));
    set.spawn(run(trie, words, recv));

    let res = set.join_next().await.expect("Known non-empty set");
    set.shutdown().await;

    let final_sentence = res??;

    println!("{:?}", final_sentence);

    Ok(())

    // match &env::args().collect::<Vec<_>>().as_slice() {
    //     &[_, dict, ref chords @ ..] => {

    //         let chords = chords
    //             .iter()
    //             .map(|x| parse_steno_chord(x).map(steno_keys_to_sounds));

    //         let before = std::time::Instant::now();
    //         for chord in chords {
    //             println!("{:?}", chord);
    //             let chord = chord?;
    //             state.step(&entries, &words, &chord, &mut weights, |a, b| {
    //                 fetch_weights(&client, a, b)
    //             })?;
    //         }
    //         let after = std::time::Instant::now();

    //         let (prob, sentence) = state.likeliest_sentence();

    //         println!("{:?}", after - before);
    //         // println!("{:?}", state);
    //         println!("{:?}", weights);
    //         println!("{} {:?}", prob, sentence);
    //         Ok(())
    //     }
    //     _ => {
    //         println!("Need at least cmudict as an argument");
    //         exit(1);
    //     }
    // }
}

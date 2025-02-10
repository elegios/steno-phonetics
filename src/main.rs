use serde::{Deserialize, Serialize};
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    env, fmt,
    fs::File,
    io::{self, BufRead},
    iter, mem,
    process::exit,
    rc::Rc,
};
use trie_rs::{
    inc_search::IncSearch,
    map::{Trie, TrieBuilder},
};

use enumset::{EnumSet, EnumSetType};

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
    fn from_line(line: &str) -> Result<WordEntry, String> {
        let mut iter = line.split_ascii_whitespace();
        if let Some(word) = iter.next() {
            let word = word.trim_end_matches("(1)").trim_end_matches("(2)");
            return Result::Ok(WordEntry {
                word: word.to_string(),
                sounds: iter
                    .map(|x| Sound::from_str(x).expect("Valid sound"))
                    .collect(),
            });
        }
        Result::Err("Empty line".to_owned())
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

fn parse_steno_chord(chord: &str) -> Option<EnumSet<StenoKey>> {
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

#[derive(Debug)]
struct FinishedBackLink<'a> {
    prev: Rc<State<'a>>,
    word_alternatives: &'a [String],
}

enum State<'a> {
    Finished {
        links: Vec<FinishedBackLink<'a>>,
        id: u32,
    },
    Unfinished {
        prev: Rc<State<'a>>,
        search: IncSearch<'a, Sound, usize>,
        id: u32,
    },
}

impl<'a> fmt::Debug for State<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Finished { links, id } => write!(f, "Finished {{id: {}, links: {:?}}}", id, links),
            Self::Unfinished { prev, search, id } => write!(
                f,
                "Unfinished {{id: {}, prev: {:?}, search: {}}}",
                id,
                prev,
                search.prefix_len()
            ),
        }
    }
}

impl<'a> State<'a> {
    fn new(ids: &mut u32) -> State<'a> {
        let id = *ids;
        *ids += 1;
        Self::Finished { links: vec![], id }
    }

    fn step(
        self: Rc<Self>,
        entries: &'a Trie<Sound, usize>,
        words: &'a Vec<Vec<String>>,
        chord: &[Sound],
        finished: &mut Vec<FinishedBackLink<'a>>,
        unfinished: &mut Vec<Rc<Self>>,
        ids: &mut u32,
    ) {
        let mut search = match &*self {
            Self::Finished { .. } => entries.inc_search(),
            Self::Unfinished { search, .. } => search.clone(),
        };

        let answer = match search.query_until(chord) {
            Err(_) => return,
            Ok(answer) => answer,
        };

        if answer.is_match() {
            finished.push(FinishedBackLink {
                word_alternatives: &words[*search.value().unwrap()],
                prev: self.clone(),
            })
        }

        if answer.is_prefix() {
            let id = *ids;
            *ids += 1;
            unfinished.push(Rc::new(Self::Unfinished {
                search,
                prev: self,
                id,
            }))
        }
    }

    fn is_finished(&self) -> bool {
        match self {
            Self::Finished { .. } => true,
            _ => false,
        }
    }

    fn iter_partial_sentences(&self) -> Box<dyn Iterator<Item = Vec<&str>> + '_> {
        match self {
            State::Finished { links, .. } => {
                if links.is_empty() {
                    Box::new(iter::once(vec![]))
                } else {
                    let it = links.iter().flat_map(|x| {
                        x.word_alternatives.iter().flat_map(|w| {
                            x.prev.iter_partial_sentences().map(|mut sentence| {
                                sentence.push(w);
                                sentence
                            })
                        })
                    });
                    Box::new(it)
                }
            }
            State::Unfinished { prev, .. } => prev.iter_partial_sentences(),
        }
    }

    const ABSENT_WEIGHT : f64 = -140.0;   // NOTE(vipa, 2025-02-12): In log2 space

    fn best_partial_sentence(
        &self,
        memo: &mut HashMap<u32, (f64, Vec<&'a str>)>,
        weights: &HashMap<(&str, &str), f64>, // NOTE(vipa, 2025-02-09): log weights
    ) -> u32 {
        let id = match self {
            Self::Finished { id, .. } => *id,
            Self::Unfinished { id, .. } => *id,
        };

        let result = match self {
            Self::Finished { links, .. } if links.is_empty() => (0.0, vec![]),
            Self::Finished { links, .. } => {
                let mut res = (f64::MIN, vec![]);
                for l in links {
                    let prev = l.prev.best_partial_sentence(memo, weights);
                    let (prev_weight, sentence) = memo.get(&prev).unwrap();
                    let mut sentence = sentence.clone();
                    let prev_word : &str = sentence.last().map_or("_START_", |x| x);
                    let (word, word_weight) = l.word_alternatives.iter()
                        .map(|w| (w, weights.get(&(prev_word, w)).map_or(Self::ABSENT_WEIGHT, |x| *x)))
                        .max_by(|l, r| l.1.total_cmp(&r.1))
                        .expect("non-empty list");
                    if prev_weight + word_weight > res.0 {
                        sentence.push(word);
                        res = (prev_weight + word_weight, sentence);
                    }
                }
                res
            }
            State::Unfinished { prev, .. } => {
                let prev = prev.best_partial_sentence(memo, weights);
                memo.get(&prev).unwrap().to_owned()
            },
        };

        memo.insert(id, result);
        id
    }

    fn bigrams(self: &Rc<Self>) -> HashSet<(&str, &str)> {
        let mut marked = HashMap::new();
        let mut requested = HashSet::new();
        self.explore(&mut marked, &mut requested);
        return requested;
    }

    fn explore(
        &self,
        marked: &mut HashMap<u32, Vec<&'a str>>,
        requested: &mut HashSet<(&'a str, &'a str)>,
    ) -> u32 {
        let id = match self {
            Self::Finished { id, .. } => *id,
            Self::Unfinished { id, .. } => *id,
        };
        if marked.contains_key(&id) {
            return id;
        }

        let result = match self {
            Self::Finished { links, .. } if links.is_empty() => vec!["_START_"],
            Self::Finished { links, .. } => {
                let mut ret = vec![];
                for l in links {
                    let prev = l.prev.explore(marked, requested);
                    let prev = marked.get(&prev).unwrap();
                    ret.extend(l.word_alternatives.iter().map(|x| x.as_str()));

                    for p in prev {
                        for w in l.word_alternatives {
                            requested.insert((p, w));
                        }
                    }
                }
                ret
            }
            Self::Unfinished { prev, .. } => {
                let prev = prev.explore(marked, requested).to_owned();
                marked.get(&prev).unwrap().to_owned()
            }
        };

        marked.insert(id, result);
        id
    }
}

#[derive(Serialize)]
struct NGramRequest {
    queries: Vec<String>
}

#[derive(Deserialize, Debug)]
#[repr(transparent)]
struct NGramToken {
    text: String
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct NGram {
    // id: String,
    // abs_total_match_count : i64,
    rel_total_match_count : f64,
    tokens : Vec<NGramToken>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct NGramResult {
    query_tokens : Vec<NGramToken>,
    ngrams : Option<Vec<NGram>>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "camelCase")]
struct NGramResponse {
    results : Vec<NGramResult>
}

fn main() {
    match &env::args().collect::<Vec<_>>().as_slice() {
        &[_, dict, ref chords @ ..] => {
            let file = File::open(dict).expect("Missing file probably");
            let entries = io::BufReader::new(file)
                .lines()
                .filter_map(|x| x.ok())
                .filter(|x| !x.starts_with(";;;"))
                .map(|x| WordEntry::from_line(&x).expect("Correct word entry"));

            let mut words_by_sounds: BTreeMap<_, Vec<_>> = BTreeMap::new();
            for e in entries {
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

            let chords = chords
                .iter()
                .map(|x| parse_steno_chord(x).expect("Correct chord"))
                .map(|x| steno_keys_to_sounds(&x));

            let mut node_ids = 0;
            let mut state = vec![Rc::new(State::new(&mut node_ids))];
            let mut next_state = vec![];

            let before = std::time::Instant::now();
            for chord in chords {
                let mut finished = vec![];
                println!("{:?}", chord);
                for s in state.drain(..) {
                    s.step(
                        &entries,
                        &words,
                        &chord,
                        &mut finished,
                        &mut next_state,
                        &mut node_ids,
                    )
                }
                if !finished.is_empty() {
                    let id = node_ids;
                    node_ids += 1;
                    next_state.push(Rc::new(State::Finished {
                        links: finished,
                        id,
                    }));
                }
                mem::swap(&mut state, &mut next_state);
            }
            let after = std::time::Instant::now();

            let finished = state.iter()
                .find(|x| x.is_finished())
                .expect("no valid sentence");

            let original_bigrams : Vec<_> = finished.bigrams()
                .into_iter()
                .collect();
            let bigrams = original_bigrams
                .iter()
                .map(|(w1, w2)| format!("{} {}", w1, w2))
                .collect();

            let client = reqwest::blocking::Client::new();
            println!("{:?}", bigrams);
            let result : NGramResponse = client.post("https://api.ngrams.dev/eng/batch?flags=cr")
                .json(&NGramRequest {queries: bigrams})
                .send()
                .expect("Requesting ngram stuff works")
                // .text()
                // .expect("There's text");
                .json()
                .expect("Properly formatted json");
            let mut weights = HashMap::new();
            for (pair, result) in original_bigrams.into_iter().zip(result.results.into_iter()) {
                if let Some(x) = result.ngrams.unwrap_or_default().first() {
                    weights.insert(pair, x.rel_total_match_count.log2());
                }
            }
            let mut memo = HashMap::new();

            let sentence = finished.best_partial_sentence(&mut memo, &weights);
            let (prob, sentence) = memo.get(&sentence).unwrap();

            let results: Vec<_> = state
                .iter()
                .filter(|x| State::is_finished(x))
                .flat_map(|x| State::iter_partial_sentences(x))
                .collect();

            println!("{:?}", after - before);
            // println!("{:?}", state);
            println!("{:?}", weights);
            println!("{} {:?}", prob, sentence);
        }
        _ => {
            println!("Need at least cmudict as an argument");
            exit(1);
        }
    }
}

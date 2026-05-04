# Conceptual Overview of Included Norms

This document outlines the set of lexical and semantic norms included in the dataset and clarifies what each measure represents. It brings together constructs from different areas of psycholinguistics—covering familiarity, processing, meaning, embodiment, affect, and lexical form—and describes how they are defined and typically interpreted. The goal is to provide a clear reference for using these variables, while noting where related measures overlap and where they capture distinct aspects of word knowledge.

## Learning and Lexical Availability

Age of acquisition (`aoa`) estimates when a word is first understood. It is often treated as a developmental property of lexical knowledge: early-acquired words tend to be more entrenched, more fluent, and easier to access. Age of acquisition overlaps with frequency and prevalence, but it is not reducible to either. A word can be common in adult text but learned late, or relatively infrequent but known early through childhood experience.

Word prevalence (`prevalence_pknown`, `prevalence_score`) estimates how widely a word is known across speakers. This differs from token frequency. Frequency asks how often a word occurs in a corpus; prevalence asks how many people know it. This is especially useful for stimulus selection because rare words are not all equally obscure, and common-looking words may vary in population-level familiarity.

The English Lexicon Project measures (`lexdec_rt`, `lexdec_naming_rt`) are behavioral indices of lexical access. Lexical decision time reflects how quickly readers judge a letter string as a word, while naming latency reflects how quickly they can read the word aloud. These measures are downstream outcomes of many other lexical properties, including frequency, age of acquisition, orthographic/phonological form, semantic richness, and affective or sensorimotor associations.

## Semantic Grounding and Imagery

Concreteness (`concreteness`) measures how much a word's meaning refers to something that can be directly experienced through the senses or action. Concrete words such as object names tend to have more perceptual grounding; abstract words depend more on language, concepts, relations, or definitions.

Imageability (`imageability`) measures how easily a word gives rise to a mental image. It is closely related to concreteness, but the two are not the same. Some abstract or emotional words can be imageable, and some concrete referents may be relatively difficult to picture. Imageability is about evoked mental imagery, not simply whether a referent exists in the physical world.

Sensory experience rating (`ser`) broadens the idea of imagery beyond visual imageability. It asks how strongly a word evokes sensory or perceptual experience across modalities. This is conceptually close to imageability and sensorimotor strength, but it is more general than "can I picture it?" and can include sound, taste, touch, smell, and other sensory impressions.

## Sensorimotor and Embodied Meaning

The Lancaster sensorimotor norms describe the perceptual and action systems associated with a concept. The perceptual dimensions (`lancaster_visual`, `lancaster_auditory`, `lancaster_haptic`, `lancaster_gustatory`, `lancaster_olfactory`, `lancaster_interoceptive`) estimate how strongly the concept is experienced through different sensory modalities. The action dimensions (`lancaster_hand_arm`, `lancaster_foot_leg`, `lancaster_mouth`, `lancaster_head`, `lancaster_torso`) estimate bodily effector involvement.

Body-object interaction (`boi`) is a related but more specific construct: how easily the body can physically interact with the referent of a word. It is often high for manipulable objects and lower for large, distant, diffuse, or abstract referents. BOI overlaps with concreteness and hand/arm sensorimotor strength, but it targets affordance for bodily interaction rather than general perceptual vividness.

The verbs-in-space dimensions (`vis_upwrd`, `vis_dwnwrd`, `vis_vert`, `vis_left`, `vis_right`, `vis_horiz`, `vis_toward`, `vis_away`) capture spatial schemas associated with actions. These dimensions are useful for verbs whose meanings imply direction, orientation, or motion. They extend embodied semantics from object properties and sensory modalities into action structure and spatial representation.

## Affect, Evaluation, and Social Meaning

Valence, arousal, and dominance (`valence`, `arousal`, `dominance`) capture affective meaning. Valence measures pleasantness, arousal measures emotional activation or intensity, and dominance measures the sense of control or power evoked by a word. These dimensions are related but separable: a word can be unpleasant and high-arousal, pleasant and low-arousal, or emotionally intense without implying personal control.

Humor (`humor`) captures perceived funniness or humorous association. It is adjacent to affect, especially valence and arousal, but it is not simply positivity. Some humorous words may be negative, absurd, taboo, playful, or surprising. This makes humor useful as a distinct evaluative and pragmatic dimension.

Socialness (`socialness`) measures how much a word's meaning concerns people, social interaction, roles, institutions, norms, groups, or social ideas. It is not the same as concreteness or valence: social concepts can be abstract, concrete, positive, negative, emotionally neutral, or emotionally loaded. Socialness helps separate interpersonal and cultural meaning from general semantic richness or affect.

Gender connotation (`gender_femininity`) measures perceived masculine-to-feminine association. This is a culturally mediated semantic dimension rather than a grammatical property. It can be useful for studying social meaning, stereotype content, political language, or distributional biases in lexical materials.

## Lexical Form, Polysemy, and Supplementary Structure

Wordset-derived measures (`n_defs`, `n_pos`) describe dictionary structure at the word-form level. Definition count is a rough indicator of semantic richness or polysemy, while part-of-speech count captures grammatical flexibility. The wide data also includes `ws_pos`, a semicolon-separated list of dictionary POS labels, and `wordset_index` keeps the POS-specific definition counts. These are reference metadata, not human ratings. Because most norming studies present words without sentence context or explicit sense disambiguation, POS-specific dictionary entries should not be interpreted as identifying which sense or grammatical use was rated.

CMU-derived measures (`n_phones`, `n_syllables`) describe phonological form. They provide length and pronunciation information that can matter for naming, memory, speech production, and stimulus matching. The wide data also keeps the primary ARPABET transcription where available.

GloVe embeddings (`glove50`) provide distributional semantic vectors for words in the compiled vocabulary. These are not norming ratings, but they offer a complementary corpus-based representation of lexical meaning. They can be used to estimate semantic similarity, find neighbors, or compare human-rated semantic dimensions with distributional structure.

## How the Constructs Relate

Several included norms cluster around broad conceptual families:

| Family | Included measures | Common question |
|---|---|---|
| Lexical availability | `aoa`, `prevalence_pknown`, `prevalence_score`, frequency covariates | How available or familiar is this word to speakers? |
| Lexical access | `lexdec_rt`, `lexdec_naming_rt` | How quickly is this word recognized or produced aloud? |
| Semantic grounding | `concreteness`, `imageability`, `ser` | How much does the word evoke perceptual or imagistic content? |
| Embodied meaning | Lancaster dimensions, `boi`, verbs-in-space dimensions | How is the word tied to sensation, action, body, or space? |
| Affective meaning | `valence`, `arousal`, `dominance`, `humor` | What emotional or evaluative response does the word evoke? |
| Social meaning | `socialness`, `gender_femininity` | How much social or cultural meaning is carried by the word? |
| Formal lexical structure | `n_defs`, `n_pos`, `n_phones`, `n_syllables`, `cmu_arpabet` | What structural properties does the lexical item have? |
| Distributional semantics | `glove50` | What does corpus context suggest about semantic similarity? |

These groupings are interpretive rather than fixed. For example, humor is affective but also social and pragmatic; BOI is sensorimotor but also related to concreteness; prevalence is lexical availability but often predicts lexical access; imageability sits between semantic grounding and sensory experience. The value of the unified resource is that these overlaps can be modeled directly rather than treated as interchangeable labels.

//---------------------------------------------------------------------------
//
//  Program:     UnitXml.pas in MidiAndMusicXmlPlayer.exe and
//               MidiAndMusicXmlPlayer.app
//
//  Project:     MidiAndMusicXmlPlayer.exe and MidiAndMusicXmlPlayer.app
//
//  Purpose:     To convert a MusicXml file into midi-events
//
//
//  Compilation: Compile with Delphi 4 (MidiAndMusicXmlPlayer.dpr) or
//               Lazarus (MidiAndMusicXmlPlayer.lpi)
//               or Delphi XE (MidiAndMusicXmlPlayer.dproj)
//
//  Description: A state machine collects data and produces an array NoteInfo
//               containing the notes for each time division. The function
//               NoteInfoWrite produces the data for midi (global MidiData array)
//               in memory.
//               Each MusicXml part becomes a midi track. In some cases more
//               than one instrument can be present and this results in a track
//               with more than one channel.
//---------------------------------------------------------------------------

unit UnitXml;

{$ifdef FPC}
{$MODE Delphi}
{$endif}
interface

uses
{$ifdef Windows}
{$ifndef FPC}
     Windows,
{$endif}
{$endif}

{$ifdef FPC}
     FileUtil,
{$endif}
     SysUtils,
     UnitMessage,
     UnitMidiDefinitions;

//
// MusicXml state machine: Normal principle is that a state is coupled
// to the symbol. E.g. reading the symbol <score-partwise>
// (XmlStateScorePartwiseBegin) will set the state (variable SystemState)
// to XmlStateScorePartwiseBegin. A few exceptions exist: Some symbols
// may occur in different states. Most important is <duration>
// (XmlStateDurationBegin). Therefore the state will not be set to
// XmlStateDurationBegin but to none of these: XmlStateDurationPitchBegin,
// XmlStateDurationRestBegin,XmlStateDurationBackupBegin or
// XmlStateDurationForwardBegin depending on the previous state.
//

//
// Music Xml symbols except XmlStateIdentifyUtf, XmlStateError and XmlStateEnd
// XmlStateIdentifyUtf denotes the state before you know what Unicode encoding
// is used
// XmlStateError denotes an unrecoverable error condition
// XmlStateEnd denotes that we have reached end-of-file
//
type TXmlState=(XmlStateIdentifyUtf,   // Special state before Unicode is identified
                XmlStateBegin,         // Undefined symbol
                XmlStateVersion,       // MusicXml version definition
                XmlStateDocType,       // MusicXml doc type definition
                // And all the real MusicXml symbols <...>
                XmlStateScorePartwiseBegin,XmlStateScorePartwiseEnd,
                XmlStateScorePartBegin,XmlStateScorePartEnd,
                XmlStateScoreInstrumentBegin,XmlStateScoreInstrumentEnd,
                XmlStateInstrumentNameBegin,XmlStateInstrumentNameEnd,

                XmlStateMidiInstrumentBegin,XmlStateMidiInstrumentEnd,
                XmlStateMidiProgramBegin,XmlStateMidiProgramEnd,
                XmlStateMidiChannelBegin,XmlStateMidiChannelEnd,

                XmlStatePartListBegin,XmlStatePartListEnd,
                XmlStatePartNameBegin,XmlStatePartNameEnd,
                XmlStatePartName,

                XmlStatePartBegin,XmlStatePartEnd,
                // When a repeat brings the fileposition back to partbegin
                XmlStateRepeatPartBegin,
                XmlStateMeasureBegin,XmlStateMeasureEnd,
                XmlStateAttributesBegin,XmlStateAttributesEnd,
                XmlStateDivisionsBegin,XmlStateDivisionsEnd,

                XmlStateFifthsBegin,XmlStateFifthsEnd,
                XmlStateModeBegin,XmlStateModeEnd,
                XmlStateBeatsBegin,XmlStateBeatsEnd,
                XmlStateBeatTypeBegin,XmlStateBeatTypeEnd,
                XmlStateSignBegin,XmlStateSignEnd,
                XmlStateLineBegin,XmlStateLineEnd,

                XmlStateCreditWordsBegin,XmlStateCreditWordsEnd,
                XmlStateCreditBegin,XmlStateCreditEnd,
                XmlStateCreditTypeBegin,XmlStateCreditTypeEnd,
                XmlStatePanBegin,XmlStatePanEnd,
                XmlStateVolumeBegin,XmlStateVolumeEnd,

                XmlStateWorkBegin,XmlStateWorkEnd,
                XmlStateWorkTitleBegin,XmlStateWorkTitleEnd,
                XmlStateMovementTitleBegin,XmlStateMovementTitleEnd,
                XmlStateTitleBegin,XmlStateTitleEnd,

                XmlStateMidiUnpitchedBegin,XmlStateMidiUnpitchedEnd,
                XmlStateUnpitchedBegin,XmlStateUnpitchedEnd,

                XmlStateKeyBegin,XmlStateKeyEnd,
                XmlStateTimeBegin,XmlStateTimeEnd,
                XmlStateClefBegin,XmlStateClefEnd,

                XmlStateTransposeBegin,XmlStateTransposeEnd,
                XmlStateChromaticBegin,XmlStateChromaticEnd,

                XmlStateNoteBegin,XmlStateNoteEnd,
                // Appears in both Notes and Directions. Extra states for that
                XmlStateStaffBegin,XmlStateStaffEnd,
                XmlStateStaffNotesBegin,XmlStateStaffNotesEnd,
                XmlStateStaffDirectionsBegin,XmlStateStaffDirectionsEnd,
                XmlStateStaffDurationBegin,XmlStateStaffDurationEnd,

                XmlStateRestBegin,
                XmlStateRestEnd,
                XmlStateRest,
                XmlStateChord,
                XmlStateGrace,
                XmlStateCue,
                XmlStatePitchBegin,XmlStatePitchEnd,
                XmlStateOctaveBegin,XmlStateOctaveEnd,
                XmlStateDuration,XmlStateDurationBegin,XmlStateDurationEnd,
                XmlStateBackupBegin,XmlStateBackupEnd,
                XmlStateForwardBegin,XmlStateForwardEnd,
                XmlStateDurationPitchBegin,XmlStateDurationPitchEnd,
                XmlStateDurationRestBegin,XmlStateDurationRestEnd,
                XmlStateDurationRest,
                XmlStateDurationBackupBegin,XmlStateDurationBackupEnd,
                XmlStateDurationForwardBegin,XmlStateDurationForwardEnd,
                XmlStateNotationsBegin,XmlStateNotationsEnd,
                XmlStateOrnamentsBegin,XmlStateOrnamentsEnd,
                XmlStateTrillMark,XmlStateTurn,XmlStateDelayedTurn,
                XmlStateInvertedTurn,
                XmlStateShake,
                XmlStateMordent,
                XmlStateInvertedMordent,
                XmlStateTremolo,
                XmlStateTremoloBegin,
                XmlStateTremoloEnd,

                XmlStateSchleifer,
                XmlStateWavyLine,
                XmlStateVibratoBegin,XmlStateVibratoEnd,

                XmlStateTechnicalBegin,XmlStateTechnicalEnd,
                XmlStateBendBegin,XmlStateBendEnd,
                XmlStateBendAlterBegin,XmlStateBendAlterEnd,
                XmlStateRelease,
                XmlStateWithBar,
                XmlStatePreBend,

                XmlStateArticulationsBegin,XmlStateArticulationsEnd,
                XmlStateStaccato,
                XmlStateStaccatissimo,
                XmlStateSpiccato,
                XmlStateBreathMark,
                XmlStateCaesura,
                XmlStateTie,XmlStateTied,
                XmlStateStemBegin,XmlStateStemEnd,

                XmlStateInstrument,

                XmlStateBarlineBegin,XmlStateBarlineEnd,
                XmlStateRepeat,
                XmlStateEnding,

                XmlStateEndingBegin,XmlStateEndingEnd,

                XmlStateDirectionBegin,XmlStateDirectionEnd,
                XmlStateDirectionTypeBegin,XmlStateDirectionTypeEnd,
                XmlStateWordsBegin,XmlStateWordsEnd,
                XmlStateSound,

                XmlStateLyricBegin,XmlStateLyricEnd,
                XmlStateTextBegin,XmlStateTextEnd,
                XmlStateSyllabicBegin,XmlStateSyllabicEnd,

                XmlStateAccidental,

                XmlStateTimeModificationBegin,XmlStateTimeModificationEnd,
                XmlStateActualNotesBegin,XmlStateActualNotesEnd,
                XmlStateNormalNotesBegin,XmlStateNormalNotesEnd,

                XmlStateStepBegin,XmlStateStepEnd,
                XmlStateAlterBegin,XmlStateAlterEnd,

                XmlStateDisplayStepBegin,XmlStateDisplayStepEnd,
                XmlStateDisplayAlterBegin,XmlStateDisplayAlterEnd,
                XmlStateDisplayOctaveBegin,XmlStateDisplayOctaveEnd,

                XmlStateSenzaMisura,
                XmlStateFermataBegin,
                XmlStateFermataEnd,
                XmlStateFermata,

                XmlStateScoop,
                XmlStatePlop,
                XmlStateDoit,
                XmlStateFalloff,
                XmlStateBreath,
                XmlStateVibrato,

                XmlStateWedge,
                XmlStateDynamicsP,
                XmlStateDynamicsPP,
                XmlStateDynamicsPPP,

                XmlStateDynamicsMP,
                XmlStateDynamicsMF,

                XmlStateDynamicsF,
                XmlStateDynamicsFF,
                XmlStateDynamicsFFF,
                XmlStateDynamicsSFZ,
                XmlStateDynamicsSF,
                XmlStateArpeggiate,

                XmlStateSegno,
                XmlStateCoda,
                XmlStateFine,
                XmlStateDacapo,
                XmlStateDacapoAlFine,
                XmlStateToCoda,
                XmlStateDalsegno,
                XmlStateDalSegnoAlCoda,
                XmlStateDacapoAlCoda,
                XmlStateDalsegnoAlFine,
                XmlStateDash,

                XmlStateError,
                XmlStateEnd);

// In preprocessing
type TDirectionState=(DirectionStateNone,
                      DirectionStateBegin,
                      DirectionStateEnd,
                      DirectionStateTypeBegin,
                      DirectionStateTypeEnd,
                      DirectionStateWordsBegin,
                      DirectionStateWordsEnd
                      );

//
// A list of all relevant xml symbol names (without "<" and ">")
//
type TXmlStateTexts=array[TXmlState] of string;
//
// A symbol in the xml input stream. Several attributes helps
// further decoding. An Xml symbol could be:
// <note default-x="98.05" default-y="-45.00"> or <measure number="1" width="214.91">
// The StateSymbol variable may have a value like: XmlStateNoteBegin
// End symbols like </note> will have StateSymbol values like: XmlStateNoteEnd
//
type TSymbol=
                  record                   // Below: Examples with field
                  StateSymbol: TXmlState;  // Symbol value (e.g. XmlStateNoteBegin)
                  InfoName: string;        // Input text. E.g. "note"
                  InfoBefore: string;      // <duration>4</duration>   (4)
                  InfoId: string;          // <part id="P2">
                  InfoNumber: string;      // <measure number="40" width="186.32">
                  InfoVersion: string;     // <?xml version="1.0" encoding="UTF-8"?>
                  InfoDocType: string;     // <!DOCTYPE score-partwise PUBLIC...
                  InfoEncoding: string;    // <?xml version="1.0" encoding="UTF-8"?>
                  InfoTypeType: string;    // <tie type="start"/>
                  InfoTimeOnly: string;    // Used for lyrics
                  InfoPage: string;        // <credit page="1">
                  InfoDefaultX: string;    // <note default-x="87.94" default-y="-25.00">
                  InfoDefaultY: string;    // <note default-x="87.94" default-y="-25.00">
                  InfoFontSize: string;    // <word-font font-family="FreeSerif" font-size="10"/>
                  InfoJustify: string;     // <credit-words ... justify="right"
                  InfoValign: string;      // <credit-words ... valign="bottom"
                  InfoDirection: string;   // <repeat direction="forward"/>
                  InfoTempo: string;       // <sound tempo="120"/>
                  InfoImplicit: string;    // <measure implicit="yes" number="0" width="178">
                  InfoVibrato: string;     // <ornaments><mordent/></ornaments>
                  InfoDummy: string;       // Fields ignored by player
                  end;

const MaxRepeatIndex = 100;        // Max level of nested repeats
const NoteInfoGraceNoteMax = 1000; // Max number of grace notes (total)
const MidiDefaultVolume=64;        // Half volume
const MidiDefaultPanorama=64;      // Middle position of sound
const DurationListMax=200;         // Max number of different <division>-definitions
const MaxMeasures=10000;           // Max number of measures
const NotePitchScale=4096;          // Scale including microtones

type TRepeatInfo =
                  record
                  RepeatStart: int64;           // File position for repeat
                  RepeatStartLineNumber: int64; // To have also correct line number
                  RepeatStartState: TXmlState;  // Saved state
                  RepeatNumber: integer;        // Number of repeats specified
                  RepeatInMeasure: boolean;     // If repeat in measure - maybe not full
                  RepeatBeginOpen: boolean;     // If repeat begin and no end, set extra
                  RepeatMeasureNumber: int64;   // The measure number
                  RepeatNoteNumber: integer;  // To identify right text(time-only)
                  RepeatTempo: integer;         // Only in MusicSubroutine
                  end;

type TAttribute = (AttributeNone,AttributeVibrato,
                   AttributeTrill,AttributeTurn,AttributeDelayedTurn,
                   AttributeInvertedTurn,AttributeWavyLine,
                   AttributeScoop,AttributePlop,AttributeDoit,AttributeFalloff,
                   AttributeBend,AttributeBendWithBar,AttributeBendPreBend,
                   AttributeBendRelease,
                   AttributeBreath,AttributeCaesura);


type TXmlStateData=
                  record
                  Utf16A: boolean;        // File is encoded as Utf16 MSB first
                  Utf16B: boolean;        // File is encoded as Utf16 Msb last
                  TrackIndex: integer;    // Index to the track header
                  SongTitle: string;      // Title, Work Title or Movement Title
                  SongTitleCodePage: string; // Same but Ansi codepage
                  CreditType: string;       // Type in e.g. <credit-type>title</credit-type>
                  MeasureNumber: integer;   // Measure as specified in <measure>
                  // Basic rule: A part becomes a midi track.
                  PartNumber: integer;      // Part as specified in <part>
                  PartNumberMax: integer;   // Max part number to handle PartMap
                  // E.g. PartMap=("P1","P2",...)
                  PartMap: array[0..Tracks] of string; // Which part goes to which track
                  // E.g. PartNames= (Violin,...)
                  PartNames: array[0..Tracks] of string;
                  PartId: string;           // E.g. "P4"
                  TrackNumber: integer;     // Midi track number. 0,1,2,3 ...
                  Tempo: int64;             // Tempo, may be changed by ritardando
                  TempoOrg: int64;          // Tempo before ritardando
          MaxVerseNumberPart: integer; // Max number of verses = played so many times
          MaxVerseNumberAll: integer; // Max number of verses = played so many times
                  InfoId: string;           // The instrument ID or other ID
                  MidiProgram: array[0..Tracks] of byte; // Is the GM instrument
                  Divisions: integer;  // Divisions according to XML
                  DivisionCount: integer; // If defined more than once - no duration reduction
                  Fifths: string;
                  Mode: string;
                  BeatsList: array[1..MaxTimeSignatures] of integer;  // Time signature upper part
                  BeatTypeList: array[1..MaxTimeSignatures] of integer;   // Time signature lower part
                  Beats: integer;          // Time signature upper part
                  BeatsMax: integer;       // Time signature upper part Max
                  BeatType: integer;       // Time signature lower part
                  Sign: string;
                  Line: string;
                  Step: string;       // The note in terms C,D,E,F,G,A,B
                  Alter: string;      // Number of half tones up/down from this
                  Octave: string;     // Octave number, increased by
                  Duration: integer;   // Duration in terms of division
                  ActualNotes: string; // NormalNotes/ActualNotes = a ratio
                  NormalNotes: string; // for the note 
                  Unpitched: array[0..Tracks] of integer;    //// ????
                  Lyric: string;      // Song text
                  LyricVerseNumber: integer; // Verse number
                  LyricTimeOnly: string; // Text to these verses
////                  CurrentVerseNumber: integer; // Just a counter
                  VerseNumberMapFirst: array[0..Tracks] of integer;
                  VerseNumberMapLast: array[0..Tracks] of integer;
                  EndingText: string; // Text for volta command

                  Syllabic: string;   // Make a "-"
                  LyricCodepage: string; // Song text converted to codepage
                  NotePause: boolean;  // NoteOn(0) or Pause(1)
                  Chord: boolean;     // This note has to be played together with preivious
                  TieStart: boolean;      // Current note is tied
                  TieStop: boolean;      // Current note is tied
                  Fermata: boolean;
                  FermataMissing: integer;
                  Staccato: boolean;
                  Arpeggiate: boolean;
                  NoteOn: array[0..Tracks,MidiNoteFirst..MidiNoteLast] of boolean;
                  // The command "backup" sets back the time. Variables to
                  // merge the notes to be played together
                  TimeChange: int64;   // A jump back or forward (backup command)
                  XmlTimeToMidiIndex: array[0..MaxNumberOfTimeDivision] of int64;
                  XmlTimeIndex: int64;
                  XmlTimeIndexMaxInMeasure: int64; // Biggest value in this measure
                  XmlTimeIndexMax: int64;
                  XmlTimeIndexLast: int64;  // Used for chord

                  XmlTimeIndexStartMerge: int64;
                  XmlTimeIndexMiddleMerge: int64;
                  XmlTimeIndexStopMerge: int64;
                  Tie: array[0..Tracks,0..127] of boolean;  // All tied notes
                  NoteDelta: int64;     // Time delta before next note
                  Stem: string;       // Note up or down - ignored here
                  Key: array[0..Tracks] of string;   // Of this track
                  Time: array[0..Tracks] of string;  //
                  // Found no information about the code for Midi Meta Event
                  // Instead ...... ////
                  Clef: array[0..Tracks] of string;  // Clef per track
                  Pan: array[0..Tracks] of 0..127; // For all tracks/channels
                  Volume: array[0..Tracks] of 0..MidiVolumeMax; // Volume per track
                  CurrentVolume: 0..MidiVolumeMax; // For current track
                  CurrentMidiVolume: 0..MidiVolumeMax; // Current track Midi volume
                  CurrentMidiPanorama: 0..MidiPanoramaMax; // Current track Midi panorama
                  CurrentVolumeStart: integer;  // Time index for start
                  CurrentVolumeUp: boolean; // true = Crescendo, false = diminuendo
          Swing: boolean;
                  MidiChannel: array[0..Tracks] of integer;  ////
                  MidiChannelNumber: 0..Channels-1;        // Count from 0
                  MidiProgramNumber: 0..MidiProgramMax;    // Instrument
                  MidiPart: array[0..Tracks] of integer;  // Not used

                  RepeatInfo: array[0..MaxRepeatIndex] of TRepeatInfo;
                  RepeatIndex: integer;  // Level of repeats (i.e index to stack)
                  RepeatIndexMin: integer;  // Start value of RepeatIndex
                  Repeats: boolean;  // If repeats exist, then this is ver number

                  RepeatInfoCurrent: TRepeatInfo;
                  RepeatInfoSegno: TRepeatInfo;
                  RepeatInfoCoda: TRepeatInfo;
                  RepeatInfoForwardCoda: TRepeatInfo; // In case the coda comes after current position
                  RepeatInfoCapo: TRepeatInfo;
                  RepeatInfoFine: TRepeatInfo;
                  RepeatInfoMeasure: TRepeatInfo;
                  MusicSubroutine: TRepeatInfo;

                  StaffNumber: integer;  // For Piano - left-right hand
                  VerseStart: int64; // File position for repeat of verse //// array?
                  VerseStartLineNumber: int64; // Line number of file position
                  VerseStartMeasureNumber: int64; // MeasureNumber for fileposition
                  VerseLast: int64;  // Last verse
                  VerseNumber: int64;   // Verse number from 1
                  VoltaText: string;    // Current volta in text (0=no volta)
          VoltaTextRepeat: string;    // Current volta in text (0=no volta)
          VoltaMax: integer;   // Max number in voltas

                  BeforeFirstMeasure: boolean;
                  BeforeFirstPart: boolean;
                  FirstPartNumber: integer;
                  NoteIsGraceNote: boolean;
                  Attribute: TAttribute;  // For Vibrato etc.
                  AttributeParm1: Integer;  // For Vibrato etc.
                  AttributeParm2: Integer;  // For Vibrato etc.
                  ParsingError: boolean;
                  // The instrument id is only used when a part has more than one instrument
                  MidiInstrumentId: string;    // An Id to correspond with notes
                  MidiInstrument: array[0..MaxNumberOfInstruments] of string;
                  MidiInstrumentPitch: array[0..MaxNumberOfInstruments] of integer;
                  MidiInstrumentProgram: array[0..MaxNumberOfInstruments] of integer;
                  MidiInstrumentChannel: array[0..MaxNumberOfInstruments] of integer;
                  MidiInstrumentPart: array[0..MaxNumberOfInstruments] of integer;
          MidiInstrumentNumber: array[0..MaxNumberOfInstruments] of string;        
                  MidiInstrumentName: array[0..MaxNumberOfInstruments] of string;
                  //// og de andra volume+pan osv.
                  MidiInstrumentMax: integer;
                  MidiInstrumentIndex: integer;

                  //// MÅSKE BRUGES ScoreInstrument... IKKE (MidiInstrument gør det!)
                  ScoreInstrumentId: string; ////
                  ScoreInstrument: array[0..MaxNumberOfInstruments] of string;
                  ScoreInstrumentPart: array[0..MaxNumberOfInstruments] of integer;
                  ScoreInstrumentNames: array[0..MaxNumberOfInstruments] of string;
                  ScoreInstrumentMax: integer;
                  ScoreInstrumentIndex: integer;
                  InstrumentName: string; // Current name

                  MidiChannelIndex: integer;

                  MaxMeasure: int64;
                  MaxIndex: int64;
                  CommonDivisor: integer;  // For time index
                  CommonMultiplicator: integer;
                  DivisionList: array[0..DurationListMax] of integer;
                  DurationList: array[0..DurationListMax] of integer;
                  DurationListIndex: integer;
                  DivisionListIndex: integer;
                  MeasureTime: int64;    // For control - check divisions
                  Transpose: integer;    // Number of steps chromatic transpose
                  NoOfTimeSignatures: integer; // Number of Time/Beats-BeatType
                  // If a fermat is in one measure, the fermat must be
                  // in all parts. If missing in part then add the delay
                  FermatInMeasurePart: array[0..MaxMeasures] of boolean;
          NoteNumber: integer;
          MoreLyric: boolean;
          MaxVersePart: array[0..MaxNumberOfInstruments] of integer;
          MaxMaxVerse: integer; // The biggest verse number of all tracks
          MaxVerseCheck: boolean; // 1 = not ready check (possible verse mismatch)
                  MicroTones: boolean;
                  Ritardando: integer;  // Number of divisions for ritardando
                  end;

type TNoteInfo = record
                 NoteOn: Set of TNoteValue;
                 NoteOff: Set of TNoteValue;
                 NoteContinue: Set of TNoteValue;
                 NoteArpeggiate: Set of TNoteValue;
                 NoteChannelOn: array[0..127] of 0..16;       // 0 = not set
                 NoteChannelOff: array[0..127] of 0..16;      // 0 = not set
                 Attribute: TAttribute;    // E.g. vibrato, palm mute etc.
                 AttributeParm1: integer;  // For Vibrato etc.
                 AttributeParm2: integer;  // For Vibrato etc.
                 Volume: integer;
                 Fermata: boolean;
                 FermataMissing: integer; ////boolean;
                 Staccato: boolean;
                 Arpeggiate: boolean;
                 NoteGraceIndex: integer;  // Current index to list of grace notes
                 NoteGraceIndexStart: integer;  // Start index to list of grace notes
                 MicroToneAlter: integer;
                 Tempo: integer;
                 MeasureNumber: integer;    // Just to show the measure number in status bar
                 SongText: string;
                 end;

type TNoteInfoGrace =
                 record
                 NoteOn: Set of TNoteValue;
                 NoteChord: boolean;
                 NoteLast: boolean;
                 end;


type THitCount=array[0..MaxEventsChannel] of integer;
// When a note is used then NoteHitCount is incremented. When a text is used
// then TextHitCount is incremented.
var NoteHitCount: THitCount;
var TextHitCount: THitCount;
var MaxHitCount: THitCount;
var NumberOfVerseCount: integer=0;
var VerseCheckCount: integer;  // First time the number of verses may be wrong
                               // but second time it should be ok

var NoteInfoGraceIndex: integer;
var NoteInfoGraceIndexStart: integer;

var FermatInMeasureAll: array[0..MaxMeasures] of boolean;
var VoltaDiscontinueInMeasure: array[0..MaxMeasures] of boolean;

//// TEST AGAINST LIMIT
NoteInfoGrace: array[0..NoteInfoGraceNoteMax] of TNoteInfoGrace;

const InterNotePauseLength = 2;       // Short pause betweeen notes
const MultFactorDefault = 480;  // Convertion to Midi Time  //// Hvordan?
const NumberOfTimeSamples = 2000000; // Absolute max length of music

type TNoteInfoData = array[0..NumberOfTimeSamples] of TNoteInfo;

// Handle Segno, Coda etc.
type TDirectionElement = (DirectionNone, DirectionSegno,DirectionCoda,DirectionFine,
                   DirectionDacapo,DirectionToCoda,DirectionDalSegno,DirectionDalSegnoAlCoda,
                   DirectionDacapoAlFine,
                   DirectionDalsegnoAlFine,
                   DirectionDacapoAlCoda);

type TDirection = set of TDirectionElement;

var MeasureDirections: array[0..MaxMeasures] of TDirection;

var CodaNoteNumber: int64;

//// Hvor bliver den initialiseret????

procedure ReadXmlToMidiFile(FileName: string; var MidiData: TMidiData);

implementation

var DataInCount: int64;  // Number of bytes read for readoperation, i.e. eof=0
    XmlDataInCount: integer;  // Number of bytes read for readoperation, i.e. eof=0
    DataIn: THandle;     // Handle for the Music-XML input file
    XmlStateTexts: TXmlStateTexts;
    XmlState: TXmlState;
    XmlStateData: TXmlStateData;
    XmlLineNumber: int64;    // In Xml file(Info for error messages)
    PNoteInfo: ^TNoteInfoData = nil;  // Array containing all note info (in time divisions)
    XmlDelta: int64;

    //// Only for test !!!!
    StateDirectionCounter: int64 = 0;
    DirectionLineNumber: int64 = 0;

////
function StringToVerseNumbers(s: string): VerseNumbers;

var i: integer;
    n: VerseNumber;
    v: VerseNumbers;

  begin
  n:=0;
  v:=[];
  for i:=1 to Length(s) do
    begin
    if s[i] in ['0'..'9'] then n:=n*10+ord(s[i])-ord('0')
    else if n>0 then begin v:=v+[n]; n:=0 end
    else n:=0;
    end;
  if n>0 then begin v:=v+[n]; n:=0 end;
  StringToVerseNumbers:=v;
  end;

function MaxVerseNumber(v: VerseNumbers): integer;

var m: integer;
    i: integer;

  begin
  m:=0;
  for i:=1 to 255 do if i in v then m:=i;
  MaxVerseNumber:=m;
  end;


//---------------------------------------------------------------------------
//
//     Function:      NoteInfoClear
//
//     Purpose:       To the whole structure for notes (on and of etc.
//                    Initialise all data. Cleared for every new part
//
//     Parameters:    none
//
//     Returns:       void
//
//     Note:          no remarks
//
//---------------------------------------------------------------------------

procedure NoteInfoClear;

var i: integer; // Loop to clear Fermat information

  begin
  if PNoteInfo=nil then New(PNoteInfo);
{$define OptimiseClear}
{$ifdef OptimiseClear}
  FillChar(PNoteInfo^,sizeof(TNoteInfo)*(XmlStateData.XmlTimeIndexMax+1),0);
{$else}
  for i:=0 to XmlStateData.XmlTimeIndexMax do
    begin
    PNoteInfo^[i].NoteOn:=[];
    PNoteInfo^[i].NoteOff:=[];
    PNoteInfo^[i].NoteContinue:=[];
    PNoteInfo^[i].NoteArpeggiate:=[];
    PNoteInfo^[i].Volume:=MidiDefaultVolume;
    PNoteInfo^[i].SongText:='';
    PNoteInfo^[i].Attribute:=AttributeNone;
    PNoteInfo^[i].AttributeParm1:=0;
    PNoteInfo^[i].AttributeParm2:=0;
    PNoteInfo^[i].Fermata:=false;
    PNoteInfo^[i].FermataMissing:=false;
    PNoteInfo^[i].Staccato:=false;
    PNoteInfo^[i].Arpeggiate:=false;
    PNoteInfo^[i].NoteGraceIndex:=0;
    PNoteInfo^[i].NoteGraceIndexStart:=0;

    for j:=0 to 127 do
      begin                       
      PNoteInfo^[i].NoteChannelOn[j]:=0;
      PNoteInfo^[i].NoteChannelOff[j]:=0;
      end;
    end;
{$endif}
  for i:=0 to XmlStateData.XmlTimeIndexMax do
    begin
    PNoteInfo^[i].Attribute:=AttributeNone; //// ????
    PNoteInfo^[i].SongText:='';
    end;
  XmlStateData.XmlTimeIndex:=0;
  for i:=0 to MaxMeasures do XmlStateData.FermatInMeasurePart[i]:=false;
  XmlStateData.NoteNumber:=0;
  XmlStateData.RepeatInfoSegno.RepeatNumber:=0;
  XmlStateData.RepeatInfoCoda.RepeatNumber:=0;
  XmlStateData.RepeatInfoCapo.RepeatNumber:=0;
  end;

//---------------------------------------------------------------------------
//
//     Function:      AlterStringToInt
//
//     Purpose:       To convert a string to integer multiplied by 256
//
//     Parameters:    e = String containing the number
//
//     Returns:       the integer value of the string
//
//     Note:          Even empty string will just return 0.
//
//---------------------------------------------------------------------------

function AlterStringToInt(s: string): int64;

var i,j: integer;       // Temporary loop variables
    minus: boolean;     // Set when number is negative
    n: int64;           // The number
    ready: boolean;     // To terminate loop when number is passed
    starting: boolean;  // To check minus comes first
    commaposition: integer; // To scale number if decimals are after dot

  begin
  commaposition:=0;
  while (Length(s)>0) and (not (s[Length(s)] in ['0'..'9','.'])) do
    s:=copy(s,1,Length(s)-1);
  while (Length(s)>0) and (not (s[1] in ['0'..'9','.','-'])) do
    s:=copy(s,2,Length(s)-1);

  // Avoid overflow, limit to 16 digits
  if Length(s)>16 then s:=copy(s,1,16);

  minus:=false;
  ready:=false;
  starting:=true;

  n:=0;
  for i:=1 to length(s) do
  if starting and (s[i]='-') then
    minus:=true
  else if (s[i] in ['0'..'9']) then
    begin
    starting:=false;
    if (not ready) and (commaposition>0) then n:=10*n+ord(s[i])-ord('0')
    end
  else if s[i]='.' then
    begin
    commaposition:=i;
    end
  else
    begin
    n:=n*NotePitchScale;
    for j:=1 to i-commaposition do n:=n div 10;
    ready:=true;
    end;

  if not ready then
    begin
    n:=n*NotePitchScale;
    if commaposition>0 then
    for j:=1 to Length(s)-commaposition do n:=n div 10;
    end;
  if minus then n:=-n;
  AlterStringToInt:=n;
  end;


//---------------------------------------------------------------------------
//
//     Function:      StringToInt
//
//     Purpose:       To convert a string to integer but with no exceptions
//
//     Parameters:    e = String containing the number
//
//     Returns:       the integer value of the string
//
//     Note:          Even empty string will just return 0
//
//---------------------------------------------------------------------------

function StringToInt(s: string): int64;

var i: integer;
    minus: boolean;
    n: integer;
    ready: boolean;
    starting: boolean;

  begin
  while (Length(s)>0) and (not (s[Length(s)] in ['0'..'9','.'])) do
    s:=copy(s,1,Length(s)-1);
  while (Length(s)>0) and (not (s[1] in ['0'..'9','.','-'])) do
    s:=copy(s,2,Length(s)-1);

  minus:=false;
  ready:=false;
  starting:=true;

  n:=0;
  for i:=1 to length(s) do
  if starting and (s[i]='-') then
    minus:=true
  else if s[i] in ['0'..'9'] then
    begin
    starting:=false;
    if not ready then n:=10*n+ord(s[i])-ord('0')
    end
  else {if s[i]='.' then} ready:=true;
  if minus then n:=-n;
  StringToInt:=n;
  end;

//---------------------------------------------------------------------------
//
//     Function:      Log2
//
//     Purpose:       Compute the logaritm (needed by midi - power of 2)
//
//     Parameters:    i = number to be converted
//
//     Returns:       log2(i)
//
//     Note:          Used in lower part of time signature for Midi
//
//---------------------------------------------------------------------------

function Log2(i: integer): integer;

var r: integer;
    j: integer;

  begin
  r:=0;
  j:=1;
  if i>=0 then
    repeat
    inc(r);
    j:=j*2;
    until j>=i;
  Log2:=r;
  end;

//---------------------------------------------------------------------------
//
//     Function:      XmlError
//
//     Purpose:       Use a message window to show error message
//
//     Parameters:    e = Error string, the text to be written
//
//     Returns:       void
//
//     Note:          none
//
//---------------------------------------------------------------------------

procedure XmlError(e: string);

  begin
  SystemState:=MidiError;
  if not XmlStateData.ParsingError then
    begin
    XmlStateData.ParsingError:=true;
    FormMessage.ErrorMessage(LinguaTextError,e+LinguaTextXmlParsingErrorIn+FileName+LinguaTextXmlLine+IntToStr(XmlLineNumber));
    XmlState:=XmlStateError;
    end;
  end;


{$ifdef FPC}

//---------------------------------------------------------------------------
//
//     Function:      UniToCodepage (for Lazarus)
//
//     Purpose:       Dummy function when compiling to Lazarus
//                    Lazarus uses Utf8 and no conversion to Code Page is
//                    needed
//
//     Parameters:    utf8 = the string to be converted
//
//     Returns:       The same string
//
//     Note:          none
//
//---------------------------------------------------------------------------

function UtfToCodepage(utf8: string): string;

  begin
  UtfToCodepage:=utf8;
  end;

{$else}

//---------------------------------------------------------------------------
//
//     Function:    UtfToCodepage
//
//     Purpose:     To convert an Utf8 string to a Code Page string
//
//     Parameters:  utf8 = a string to be converted to code page 865
//
//     Returns:     The converted string
//
//     Notes:       Used by Delphi 4. Lazarus uses Utf8 directly
//
//---------------------------------------------------------------------------

function UtfToCodepage(utf8: string): string;

var l: integer;
    s: string;
    i: integer;
    c1: integer;
    c2: integer;
    c3: integer;
    c4: integer;
    c: integer;

  begin
  if (not XmlStateData.Utf16A) and (not XmlStateData.Utf16B) then
    begin
    l:=Length(utf8);
    i:=1;
    s:='';
    while i<=l do
      begin
      if ord(utf8[i])<128 then
        begin
        s:=s+UniToCodepage(ord(utf8[i]));
        inc(i);
        end
      else if (i<l) and (ord(utf8[i])<224) then
        begin
        c1:=ord(utf8[i]);
        c2:=ord(utf8[i+1]);
        c:=(c2 and 63)+((c1 and 31) shl 6);
        s:=s+UniToCodePage(c);
        inc(i);
        inc(i);
        end
      else if (i<l-1) and (ord(utf8[i])<240) then
        begin
          begin
          c1:=ord(utf8[i]);
          c2:=ord(utf8[i+1]);
          c3:=ord(utf8[i+2]);
          c:=(c3 and 63)+((c2 and 63) shl 6)+((c1 and 15) shl 12);
          s:=s+UniToCodepage(c);
          inc(i);
          inc(i);
          inc(i);
          end
        end
      else if i<l-2 then
        begin
          begin
          c1:=ord(utf8[i]);
          c2:=ord(utf8[i+1]);
          c3:=ord(utf8[i+2]);
          c4:=ord(utf8[i+2]);
          c:=(c4 and 63)+((c3 and 63) shl 6)+((c2 and 63) shl 12)+((c1 and 15) shl 18);
          s:=s+UniToCodepage(c);
          inc(i);
          inc(i);
          inc(i);
          end
        end
      else
        begin
////        XmlError('Undecoded utf8 char: '+IntToStr(ord(utf8[i])));
        s:=utf8;
        i:=l+1; //// STOP
        end;
      end;
    UtfToCodepage:=s;
    end
  else
    begin
    //// Utf16. The string is not utf8, just return without conversion
    UtfToCodepage:=utf8;
    end;
  end;

{$endif}

//---------------------------------------------------------------------------
//
//     Function:    InfoDecode
//
//     Purpose:     Decode the MusicXml line for a list of definitions
//                  see the list below. E.g. "number = 1"
//
//     Parameters:  XmlInfoString = all data between "<" and ">"
//                         E.g. <measure number="1" width="214.91">
//                  InfoBefore = Information before "<"
//                         E.g.  Violin</part-name>
//     Returns:     The fields in the string converted to TSymbol-structure
//
//     Notes:
//
//---------------------------------------------------------------------------

procedure InfoDecode(XmlInfoString: string; InfoBefore: string; var Symbol: TSymbol);

var StartPos: integer;  // We test from this position
    EqualPos: integer;  // Position of the current equal-sign
    EndPos: integer;    // End of next keyword (after the equal sign)
    i: integer;         // Index to the whole text line
    InfoName: string;   // Name of the current field
    InfoValue: string;  // The value - the right side of the equal sign
    InsideQuotationMark: boolean; // Inside quotation we do not stop at space char
    x: TXmlState;       // For loop variable - search all states

  begin
  EqualPos:=MAXINT;
  InsideQuotationMark:=false;
  InfoName:='';

  // All possible decoded symbols of this common routine
  Symbol.InfoId:='';
  Symbol.InfoNumber:='';
  Symbol.InfoVersion:='';
  Symbol.InfoEncoding:='';
  Symbol.InfoTypeType:=''; ////
  Symbol.InfoTimeOnly:='';
  Symbol.InfoDocType:='';
  Symbol.InfoPage:='';
  Symbol.InfoDefaultX:='';
  Symbol.InfoDefaultY:='';
  Symbol.InfoFontSize:='';
  Symbol.InfoJustify:='';
  Symbol.InfoValign:='';
  Symbol.InfoDirection:='';
  Symbol.InfoTempo:='';
  Symbol.InfoImplicit:='';
  Symbol.InfoDummy:='';
  Symbol.InfoVibrato:='';

  InfoValue:='';
  // Handle the special DocType-specification
  if (Pos('!DOCTYPE',XmlInfoString)>0) then Symbol.InfoDocType:=XmlInfoString;
  // Either the command name and more info (separated with space)
  // or no more info
  StartPos:=Pos(' ',XmlInfoString);
  if StartPos>0 then
    begin
    Symbol.InfoName:=Copy(XmlInfoString,1,StartPos-1);
    // Keep symbol name with info removed
    if XmlInfoString[Length(XmlInfoString)]='/' then
      Symbol.InfoName:=Symbol.InfoName+'/';
    end
  else
    Symbol.InfoName:=XmlInfoString;

  // If a symbol is defined by equal sign, e.g. credit page="1"
  if Pos('=',XmlInfoString)>0 then
    begin
    // Some symbols come with begin-end (<SYM> </SYM>), but some are only
    // single Xml-symbols ending with "/" (<SYM/>)
    if (Length(XmlInfoString)>0) and (XmlInfoString[Length(XmlInfoString)]='/') then
      XmlInfoString:=copy(XmlInfoString,1,Length(XmlInfoString)-1);
    for i:=1 to Length(XmlInfoString) do
      begin
      if (XmlInfoString[i]='"') then InsideQuotationMark:=not InsideQuotationMark;

      if (i=Length(XmlInfoString)) or
         ((not InsideQuotationMark) and (XmlInfoString[i]=' ')) then
        begin
        // End of value - remove space if not end of string
        if XmlInfoString[i]=' ' then EndPos:=i-1 else EndPos:=i;
        if i>EqualPos then
          begin // A value was found
          InfoValue:=copy(XmlInfoString,EqualPos+1,EndPos-EqualPos);
          EqualPos:=MAXINT-1; ////     InfoDummy?
          if InfoName='id' then Symbol.InfoId:=InfoValue

          else if InfoName='version' then Symbol.InfoVersion:=InfoValue
          else if InfoName='encoding' then Symbol.InfoEncoding:=InfoValue
          else if InfoName='version' then Symbol.InfoVersion:=InfoValue
          else if InfoName='type' then Symbol.InfoTypeType:=InfoValue
          else if InfoName='page' then Symbol.InfoPage:=InfoValue
          else if InfoName='default-x' then Symbol.InfoDefaultX:=InfoValue
          else if InfoName='default-y' then Symbol.InfoDefaultY:=InfoValue
          else if InfoName='font-size' then Symbol.InfoFontSize:=InfoValue
          else if InfoName='justify' then Symbol.InfoJustify:=InfoValue
          else if InfoName='valign' then Symbol.InfoValign:=InfoValue
          else if InfoName='number' then Symbol.InfoNumber:=InfoValue
          else if InfoName='width' then Symbol.InfoDummy:=InfoValue
          else if InfoName='new-system' then Symbol.InfoDummy:=InfoValue
          else if InfoName='location' then Symbol.InfoDummy:=InfoValue
          else if InfoName='direction' then Symbol.InfoDirection:=InfoValue
          else if InfoName='attribute' then Symbol.InfoDummy:=InfoValue
          else if InfoName='dynamics' then Symbol.InfoDummy:=InfoValue
          else if InfoName='standalone' then Symbol.InfoDummy:=InfoValue
          else if InfoName='element' then Symbol.InfoDummy:=InfoValue
          else if InfoName='value' then Symbol.InfoDummy:=InfoValue
          else if InfoName='font-family' then Symbol.InfoDummy:=InfoValue
          else if InfoName='tempo' then Symbol.InfoTempo:=InfoValue
          else if InfoName='print-object' then Symbol.InfoDummy:=InfoValue
          else if InfoName='implicit' then Symbol.InfoImplicit:=InfoValue
          else if InfoName='vibrato' then Symbol.InfoVibrato:=InfoValue
          else if InfoName='time-only' then Symbol.InfoTimeOnly:=InfoValue
////          else
////            MessageBox(0,PChar(InfoName),PChar(FileName),0);
          end;
        StartPos:=i;
        end
      else if XmlInfoString[i]='=' then
        begin
        EqualPos:=i;
        InfoName:=copy(XmlInfoString,StartPos+1,EqualPos-StartPos-1);
        end;

      end;
    end;

  Symbol.StateSymbol:=XmlStateBegin;
  for x:=XmlStateBegin to XmlStateEnd do
    if (Symbol.InfoName<>'') and (Symbol.InfoName=XmlStateTexts[x]) then Symbol.StateSymbol:=x;
  if (Symbol.StateSymbol=XmlStateBegin) and (DataInCount=0) then Symbol.StateSymbol:=XmlStateEnd;
  Symbol.InfoBefore:=InfoBefore;
  end;

//---------------------------------------------------------------------------
//
//     Function:    GetFilePos
//
//     Purpose:     Another FileSeek that handles the buffered read of
//                  ReadChar (that speeds up reading)
//
//     Parameters:  h = handle of data in (the MusicXml file)
//
//     Returns:     <void>
//
//     Notes:       Uses global data: XmlDataInCount and ReadXmlBufferIndex
//
//---------------------------------------------------------------------------

function GetFilePos(h: Thandle): int64;
  begin
  GetFilePos:=FileSeek(h,0,1)-XmlDataInCount+ReadXmlBufferIndex;
  end;

//---------------------------------------------------------------------------
//
//     Function:    SetFilePosition
//
//     Purpose:     Another FileSeek that handles the buffered read of
//                  ReadChar (that speeds up reading). Zero the
//                  input buffer of ReadChar.
//
//     Parameters:  h = handle of data in (the MusicXml file)
//
//     Returns:     <void>
//
//     Notes:       Uses global data: XmlDataInCount and ReadXmlBufferIndex
//
//---------------------------------------------------------------------------

procedure SetFilePosition(h: Thandle; Offset: integer; Origin: integer);

  begin
  FileSeek(h,Offset,Origin);
  ReadXmlBufferIndex:=0;
  XmlDataInCount:=0;
  end;

//---------------------------------------------------------------------------
//
//     Function:    ReadChar
//
//     Purpose:     Another FileRead that handles buffered input.
//                  (speeds up reading).
//
//     Parameters:  c = the char to be set
//
//     Returns:     <void>
//
//     Notes:       Uses global data: XmlDataInCount and ReadXmlBufferIndex
//
//---------------------------------------------------------------------------

procedure ReadChar(var c: char);

  begin
{$define TestOptimisation}
{$ifdef TestOptimisation}
  ReadXmlBufferIndex:=ReadXmlBufferIndex and 255;
  if ReadXmlBufferIndex=0 then
    XmlDataInCount:=FileRead(DataIn,ReadXmlBuf,256);
  if ReadXmlBufferIndex<XmlDataInCount then
    begin
    c:=chr(ord(ReadXmlBuf[ReadXmlBufferIndex]) and 255);
    inc(ReadXmlBufferIndex);
    DataInCount:=1;
    end
  else
  begin
    c:=chr(0);
    DataInCount:=0;
    end;
{$else}
  DataInCount:=FileRead(DataIn,ReadXmlBuf,1);
  c:=chr(ord(ReadXmlBuf[0]) and 255);
{$endif}
  end;

//---------------------------------------------------------------------------
//
//     Function:    NextSymbolAndComments
//
//     Purpose:     Decode the Xml file. Read next symbol
//                  (Next symbol may be a comment
//
//     Parameters:  none
//
//     Returns:     The symbol
//
//     Notes:       none
//
//---------------------------------------------------------------------------

function NextSymbolAndComments: TSymbol;

var s: string;       // For building the string inside brackets
    b: string;       // Characters before symbol (some value or text)
    c: char;         // Next character
    Symbol: TSymbol; // The symbol read (to be returned)
    dummy: char;

  begin
  s:='';
  b:='';             ////
  c:=chr(0);         // The character in question
  dummy:=chr(0);     // Just to avoid warning message

    repeat  //// comments?
    if DataInCount>0 then ReadChar(c) else c:=chr(0);
    //// Hvad gør du med dette?
    if XmlStateData.Utf16A and (DataInCount>0) then ReadChar(dummy);
    if XmlStateData.Utf16B and (DataInCount>0) then ReadChar(c);
    if XmlState=XmlStateIdentifyUtf then
      begin
      if c=chr(254) then
        begin
        ReadChar(c);
        if c=chr(255) then
          begin
          // This is UTF16 (A)
          ReadChar(c);
          XmlStateData.Utf16A:=true;
          XmlState:=XmlStateBegin;
          end;
        end
      else if c=chr(255) then
        begin
        ReadChar(c);
        if c=chr(254) then
          begin
          // This is UTF16 (B)
          ReadChar(c);
          XmlStateData.Utf16B:=true;
          XmlState:=XmlStateBegin;
          end;
        end
      else
        begin
        XmlState:=XmlStateBegin;
        end;
      end;
    if ord(c)=10 then inc(XmlLineNumber);
    if (c<>'<') and (c<>chr(13)) and (c<>chr(10)) then b:=b+c;
    until (DataInCount=0) or (c='<');

  while (c<>'>') and (DataInCount>0) do
    begin
    ReadChar(c);
    //// Hvad gør du med dette?
    if XmlStateData.Utf16A and (DataInCount>0) then ReadChar(dummy);
    if XmlStateData.Utf16B and (DataInCount>0) then ReadChar(c);
    if ord(c)=10 then inc(XmlLineNumber);
    if c<>'>' then s:=s+c;
    end;

  FillChar(Symbol,sizeof(Symbol),0);
  InfoDecode(s,b,Symbol);

  NextSymbolAndComments:=Symbol;
  end;

//---------------------------------------------------------------------------
//
//     Function:    NextSymbol
//
//     Purpose:     Decode the Xml file. Read next symbol, ignore
//                  comments
//
//     Parameters:  none
//
//     Returns:     The symbol
//
//     Notes:       none
//
//---------------------------------------------------------------------------

function NextSymbol: TSymbol;

var s: TSymbol;

  begin
  // Until not a special Finale comment
    repeat
    s:=NextSymbolAndComments;
    until Pos('!--=========================================================-',s.InfoName)=0;
  NextSymbol:=s;
  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlInit
//
//     Purpose:     Initialise the texts search for and used in the Xml file
//                  and set default values of some parameters
//
//     Parameters:  none
//
//     Returns:     void
//
//     Notes:
//
//---------------------------------------------------------------------------

procedure XmlInit;

var i: integer;

  begin
  XmlStateTexts[XmlStateIdentifyUtf]:='Utf Identification';
  XmlStateTexts[XmlStateError]:='Xml State Error';
  XmlStateTexts[XmlStateBegin]:='begin';
  XmlStateTexts[XmlStateVersion]:='?xml';
  XmlStateTexts[XmlStateDocType]:='!DOCTYPE';
  XmlStateTexts[XmlStateScorePartwiseBegin]:='score-partwise';
  XmlStateTexts[XmlStateScorePartwiseEnd]:='/score-partwise';
  XmlStateTexts[XmlStateScorePartBegin]:='score-part';
  XmlStateTexts[XmlStateScorePartEnd]:='/score-part';
  XmlStateTexts[XmlStateScoreInstrumentBegin]:='score-instrument';
  XmlStateTexts[XmlStateScoreInstrumentEnd]:='/score-instrument';
  XmlStateTexts[XmlStateInstrumentNameBegin]:='instrument-name';
  XmlStateTexts[XmlStateInstrumentNameEnd]:='/instrument-name';

  XmlStateTexts[XmlStateMidiInstrumentBegin]:='midi-instrument';
  XmlStateTexts[XmlStateMidiInstrumentEnd]:='/midi-instrument';
  XmlStateTexts[XmlStateMidiProgramBegin]:='midi-program';
  XmlStateTexts[XmlStateMidiProgramEnd]:='/midi-program';
  XmlStateTexts[XmlStateMidiChannelBegin]:='midi-channel';
  XmlStateTexts[XmlStateMidiChannelEnd]:='/midi-channel';

  XmlStateTexts[XmlStatePartListBegin]:='part-list';
  XmlStateTexts[XmlStatePartListEnd]:='/part-list';
  XmlStateTexts[XmlStatePartNameBegin]:='part-name';
  XmlStateTexts[XmlStatePartNameEnd]:='/part-name';
  XmlStateTexts[XmlStatePartName]:='part-name/';

  XmlStateTexts[XmlStatePartBegin]:='part';
  XmlStateTexts[XmlStatePartEnd]:='/part';
  XmlStateTexts[XmlStateMeasureBegin]:='measure';
  XmlStateTexts[XmlStateMeasureEnd]:='/measure';
  XmlStateTexts[XmlStateAttributesBegin]:='attributes';
  XmlStateTexts[XmlStateAttributesEnd]:='/attributes';

  XmlStateTexts[XmlStateDivisionsBegin]:='divisions';
  XmlStateTexts[XmlStateDivisionsEnd]:='/divisions';

  XmlStateTexts[XmlStateFifthsBegin]:='fifths';
  XmlStateTexts[XmlStateFifthsEnd]:='/fifths';

  XmlStateTexts[XmlStateModeBegin]:='mode';
  XmlStateTexts[XmlStateModeEnd]:='/mode';

  XmlStateTexts[XmlStateBeatsBegin]:='beats';
  XmlStateTexts[XmlStateBeatsEnd]:='/beats';

  XmlStateTexts[XmlStateBeatTypeBegin]:='beat-type';
  XmlStateTexts[XmlStateBeatTypeEnd]:='/beat-type';

  XmlStateTexts[XmlStateSignBegin]:='sign';
  XmlStateTexts[XmlStateSignEnd]:='/sign';

  XmlStateTexts[XmlStateLineBegin]:='line';
  XmlStateTexts[XmlStateLineEnd]:='/line';

  XmlStateTexts[XmlStateTimeBegin]:='time';
  XmlStateTexts[XmlStateTimeEnd]:='/time';
  XmlStateTexts[XmlStateBegin]:='';

  XmlStateTexts[XmlStatePitchBegin]:='pitch';
  XmlStateTexts[XmlStatePitchEnd]:='/pitch';
  XmlStateTexts[XmlStateDurationBegin]:='duration';
  XmlStateTexts[XmlStateDurationEnd]:='/duration';
  XmlStateTexts[XmlStateDuration]:='duration/';


  XmlStateTexts[XmlStateStemBegin]:='stem';
  XmlStateTexts[XmlStateStemEnd]:='/stem';

  XmlStateTexts[XmlStateInstrument]:='instrument/';

  XmlStateTexts[XmlStateLyricBegin]:='lyric';
  XmlStateTexts[XmlStateLyricEnd]:='/lyric';
  XmlStateTexts[XmlStateTextBegin]:='text';
  XmlStateTexts[XmlStateTextEnd]:='/text';
  XmlStateTexts[XmlStateSyllabicBegin]:='syllabic';
  XmlStateTexts[XmlStateSyllabicEnd]:='/syllabic';



  XmlStateTexts[XmlStateTimeModificationBegin]:='time-modification';
  XmlStateTexts[XmlStateTimeModificationEnd]:='/time-modification';
  XmlStateTexts[XmlStateActualNotesBegin]:='actual-notes';
  XmlStateTexts[XmlStateActualNotesEnd]:='/actual-notes';
  XmlStateTexts[XmlStateNormalNotesBegin]:='normal-notes';
  XmlStateTexts[XmlStateNormalNotesEnd]:='/normal-notes';

////  XmlStateTexts[XmlStateMidiInstrumentBegin]:='midi-instrument';
////  XmlStateTexts[XmlStateMidiInstrumentEnd]:='/midi-instrument';
  XmlStateTexts[XmlStateCreditBegin]:='credit';
  XmlStateTexts[XmlStateCreditEnd]:='/credit';
  XmlStateTexts[XmlStateCreditWordsBegin]:='credit-words';
  XmlStateTexts[XmlStateCreditWordsEnd]:='/credit-words';
  XmlStateTexts[XmlStateCreditTypeBegin]:='credit-type';
  XmlStateTexts[XmlStateCreditTypeEnd]:='/credit-type';
  XmlStateTexts[XmlStatePanBegin]:='pan';
  XmlStateTexts[XmlStatePanEnd]:='/pan';
  XmlStateTexts[XmlStateVolumeBegin]:='volume';
  XmlStateTexts[XmlStateVolumeEnd]:='/volume';

  XmlStateTexts[XmlStateWorkBegin]:='work';
  XmlStateTexts[XmlStateWorkEnd]:='/work';
  XmlStateTexts[XmlStateWorkTitleBegin]:='work-title';
  XmlStateTexts[XmlStateWorkTitleEnd]:='/work-title';
  XmlStateTexts[XmlStateMovementTitleBegin]:='movement-title';
  XmlStateTexts[XmlStateMovementTitleEnd]:='/movement-title';
  XmlStateTexts[XmlStateTitleBegin]:='title';
  XmlStateTexts[XmlStateTitleEnd]:='/title';

  XmlStateTexts[XmlStateMidiUnpitchedBegin]:='midi-unpitched';
  XmlStateTexts[XmlStateMidiUnpitchedEnd]:='/midi-unpitched';

  XmlStateTexts[XmlStateUnpitchedBegin]:='unpitched';
  XmlStateTexts[XmlStateUnpitchedEnd]:='/unpitched';

  XmlStateTexts[XmlStateKeyBegin]:='key';
  XmlStateTexts[XmlStateKeyEnd]:='/key';
  XmlStateTexts[XmlStateClefBegin]:='clef';
  XmlStateTexts[XmlStateClefEnd]:='/clef';

  XmlStateTexts[XmlStateTransposeBegin]:='transpose';
  XmlStateTexts[XmlStateTransposeEnd]:='/transpose';
  XmlStateTexts[XmlStateChromaticBegin]:='chromatic';
  XmlStateTexts[XmlStateChromaticEnd]:='/chromatic';

  XmlStateTexts[XmlStateNoteBegin]:='note';
  XmlStateTexts[XmlStateNoteEnd]:='/note';

  XmlStateTexts[XmlStateStaffBegin]:='staff';
  XmlStateTexts[XmlStateStaffEnd]:='/staff';

  XmlStateTexts[XmlStateBackupBegin]:='backup';
  XmlStateTexts[XmlStateBackupEnd]:='/backup';

  XmlStateTexts[XmlStateForwardBegin]:='forward';
  XmlStateTexts[XmlStateForwardEnd]:='/forward';

  XmlStateTexts[XmlStateChord]:='chord/';
  XmlStateTexts[XmlStateGrace]:='grace/';
  XmlStateTexts[XmlStateCue]:='cue/';

  XmlStateTexts[XmlStateRestBegin]:='rest';
  XmlStateTexts[XmlStateRestEnd]:='/rest';
  XmlStateTexts[XmlStateRest]:='rest/';

  XmlStateTexts[XmlStateStepBegin]:='step';
  XmlStateTexts[XmlStateStepEnd]:='/step';

  XmlStateTexts[XmlStateNotationsBegin]:='notations';
  XmlStateTexts[XmlStateNotationsEnd]:='/notations';

  XmlStateTexts[XmlStateOrnamentsBegin]:='ornaments';
  XmlStateTexts[XmlStateOrnamentsEnd]:='/ornaments';
  XmlStateTexts[XmlStateTrillMark]:='trill-mark/';

  XmlStateTexts[XmlStateShake]:='shake/';
  XmlStateTexts[XmlStateMordent]:='mordent/';
  XmlStateTexts[XmlStateInvertedMordent]:='inverted-mordent/';
  XmlStateTexts[XmlStateTremolo]:='tremolo/';
  XmlStateTexts[XmlStateTremoloBegin]:='tremolo';
  XmlStateTexts[XmlStateTremoloEnd]:='/tremolo';
  XmlStateTexts[XmlStateSchleifer]:='schleifer/';
  XmlStateTexts[XmlStateWavyLine]:='wavy-line/';

  XmlStateTexts[XmlStateTurn]:='turn/';
  XmlStateTexts[XmlStateDelayedTurn]:='delayed-turn/';
  XmlStateTexts[XmlStateInvertedTurn]:='inverted-turn/';

  XmlStateTexts[XmlStateVibratoBegin]:='vibrato';
  XmlStateTexts[XmlStateVibratoEnd]:='/vibrato';

  XmlStateTexts[XmlStateTechnicalBegin]:='technical';
  XmlStateTexts[XmlStateTechnicalEnd]:='/technical';
  XmlStateTexts[XmlStateBendBegin]:='bend';
  XmlStateTexts[XmlStateBendEnd]:='/bend';
  XmlStateTexts[XmlStateBendAlterBegin]:='bend-alter';
  XmlStateTexts[XmlStateBendAlterEnd]:='/bend-alter';
  XmlStateTexts[XmlStateRelease]:='release/';
  XmlStateTexts[XmlStateWithBar]:='with-bar/';
  XmlStateTexts[XmlStatePreBend]:='pre-bend/';

  XmlStateTexts[XmlStateArticulationsBegin]:='articulations';
  XmlStateTexts[XmlStateArticulationsEnd]:='/articulations';

  XmlStateTexts[XmlStateTied]:='tied/';
  XmlStateTexts[XmlStateTie]:='tie/';
  XmlStateTexts[XmlStateStaccato]:='staccato/';
  XmlStateTexts[XmlStateStaccatissimo]:='staccatissimo/';
  XmlStateTexts[XmlStateSpiccato]:='spiccato/';
  XmlStateTexts[XmlStateBreathMark]:='breath-mark/';
  XmlStateTexts[XmlStateCaesura]:='caesura/';

  XmlStateTexts[XmlStateBarlineBegin]:='barline';
  XmlStateTexts[XmlStateBarlineEnd]:='/barline';
  XmlStateTexts[XmlStateRepeat]:='repeat/';
  XmlStateTexts[XmlStateEnding]:='ending/';
  XmlStateTexts[XmlStateSenzaMisura]:='senza-misura/';
  XmlStateTexts[XmlStateFermata]:='fermata/';
  XmlStateTexts[XmlStateFermataBegin]:='fermata';
  XmlStateTexts[XmlStateFermataEnd]:='/fermata';

  XmlStateTexts[XmlStateScoop]:='scoop/';
  XmlStateTexts[XmlStatePlop]:='plop/';
  XmlStateTexts[XmlStateDoit]:='doit/';
  XmlStateTexts[XmlStateFalloff]:='falloff/';
  XmlStateTexts[XmlStateBreath]:='breath/';
  XmlStateTexts[XmlStateCaesura]:='caesura/';
  XmlStateTexts[XmlStateVibrato]:='vibrato/';

  XmlStateTexts[XmlStateWedge]:='wedge/';
  XmlStateTexts[XmlStateDynamicsP]:='p/';
  XmlStateTexts[XmlStateDynamicsPP]:='pp/';
  XmlStateTexts[XmlStateDynamicsPPP]:='ppp/';

  XmlStateTexts[XmlStateDynamicsMP]:='mp/';
  XmlStateTexts[XmlStateDynamicsMF]:='mf/';

  XmlStateTexts[XmlStateDynamicsF]:='f/';
  XmlStateTexts[XmlStateDynamicsFF]:='ff/';
  XmlStateTexts[XmlStateDynamicsFFF]:='fff/';
  XmlStateTexts[XmlStateDynamicsSFZ]:='sfz/';
  XmlStateTexts[XmlStateDynamicsSFZ]:='sf/';
  XmlStateTexts[XmlStateArpeggiate]:='arpeggiate/';

  XmlStateTexts[XmlStateSegno]:='segno/';
  XmlStateTexts[XmlStateCoda]:='coda/';
  XmlStateTexts[XmlStateFine]:='fine/';
  XmlStateTexts[XmlStateDacapo]:='dacapo/';
  XmlStateTexts[XmlStateTocoda]:='tocoda/';
  XmlStateTexts[XmlStateDalsegno]:='dalsegno/';


  XmlStateTexts[XmlStateEndingBegin]:='ending';
  XmlStateTexts[XmlStateEndingEnd]:='/ending';

  XmlStateTexts[XmlStateDirectionBegin]:='direction';
  XmlStateTexts[XmlStateDirectionEnd]:='/direction';
  XmlStateTexts[XmlStateDirectionTypeBegin]:='direction-type';
  XmlStateTexts[XmlStateDirectionTypeEnd]:='/direction-type';
  XmlStateTexts[XmlStateWordsBegin]:='words';
  XmlStateTexts[XmlStateWordsEnd]:='/words';
  XmlStateTexts[XmlStateSound]:='sound/';
  XmlStateTexts[XmlStateDash]:='dashes/';

  XmlStateTexts[XmlStateAccidental]:='accidental/';

  XmlStateTexts[XmlStateOctaveBegin]:='octave';
  XmlStateTexts[XmlStateOctaveEnd]:='/octave';
  XmlStateTexts[XmlStateAlterBegin]:='alter';
  XmlStateTexts[XmlStateAlterEnd]:='/alter';

  XmlStateTexts[XmlStateDisplayOctaveBegin]:='display-octave';
  XmlStateTexts[XmlStateDisplayOctaveEnd]:='/display-octave';
  XmlStateTexts[XmlStateDisplayAlterBegin]:='display-alter';
  XmlStateTexts[XmlStateDisplayAlterEnd]:='/display-alter';
  XmlStateTexts[XmlStateDisplayStepBegin]:='display-step';
  XmlStateTexts[XmlStateDisplayStepEnd]:='/display-step';

  XmlStateTexts[XmlStateMeasureBegin]:='measure';
  XmlStateTexts[XmlStateMeasureEnd]:='/measure';
  XmlStateTexts[XmlStateEnd]:='end';


  for i:=0 to Tracks do XmlStateData.MidiChannel[i and 15]:=i and 15;
  for i:=0 to Tracks do XmlStateData.MidiProgram[i]:=0;
  for i:=0 to Tracks do XmlStateData.Volume[i]:=MidiDefaultVolume;
  for i:=0 to Tracks do XmlStateData.Pan[i]:=(i and 1)*127;
  for i:=0 to MaxMeasures do FermatInMeasureAll[i]:=false;
  for i:=0 to MaxMeasures do VoltaDiscontinueInMeasure[i]:=false;
  for i:=0 to MaxMeasures do MeasureDirections[i]:=[];
  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlMidiWriteByte
//
//     Purpose:     To write a single byte to midi data
//                  (in memory, but like file format)
//
//     Parameters:  MidiData = produced data according to the midi standard
//                  b        = the byte.
//
//     Returns:     void
//
//     Notes:       Not variable format.
//
//---------------------------------------------------------------------------

procedure XmlMidiWriteByte(var MidiData: TMidiData; b: byte);

  begin
  if MidiData.MidiDataIndexMax>=MaxFileSize then
    begin
    XmlError('The file '+FileName+' is too big');
    end
  else
    begin
  MidiData.MidiData[MidiData.MidiDataIndexMax]:=b;
  MidiData.MidiDataSelected[MidiData.MidiDataIndexMax]:=b;
  MidiData.MidiDataChannel[MidiData.MidiDataIndexMax]:=ord('0');
  inc(MidiData.MidiDataIndexMax);
  end;
  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlMidiWriteValue
//
//     Purpose:     To write a single value to midi data
//                  (in memory, but like file format)
//
//     Parameters:  MidiData = produced data according to the midi standard
//                  v        = the value. Coded according to midi standars
//                             (variable format, 7 bits per byte)
//
//     Returns:     void
//
//     Notes:       Variable format, less 128 is one byte etc.
//
//---------------------------------------------------------------------------

procedure XmlMidiWriteValue(var MidiData: TMidiData; v: int64);

var ByteNumber: integer;         // Index number of byte (reversed)
    Bytes: array[0..128] of byte; // The generated coded number
    i: integer;                   //

  begin
  if v<0 then
    XmlError('');
  ByteNumber:=0;
    repeat
    Bytes[ByteNumber]:=v and 127;
    v:=v shr 7;
    // Continuation? Set first bit
    inc(ByteNumber);
    until v=0;
  for i:=ByteNumber-1 downto 0 do
    begin
    if i>0 then XmlMidiWriteByte(MidiData,Bytes[i]+128)
    else XmlMidiWriteByte(MidiData,Bytes[i])
    end;
  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlMidiHeader
//
//     Purpose:     The produce the data to start the midi data
//
//     Parameters:  XmlStateData = data read from Xml
//                  MidiData = produced data according to the midi standard
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure XmlMidiHeader(var XmlStateData: TXmlStateData; var MidiData: TMidiData);

var
  i: integer;       // Loop through text

  begin
  MidiData.MidiDataIndexMax:=0;

  XmlMidiWriteByte(MidiData,ord('M'));      // "Midi"
  XmlMidiWriteByte(MidiData,ord('T'));      // "Track"
  XmlMidiWriteByte(MidiData,ord('h'));      // "hea-"
  XmlMidiWriteByte(MidiData,ord('d'));      // "der"
  XmlMidiWriteByte(MidiData,0);             // Header length = 6
  XmlMidiWriteByte(MidiData,0);             // Header length = 6
  XmlMidiWriteByte(MidiData,0);             // Header length = 6
  XmlMidiWriteByte(MidiData,6);             // Header length = 6
  XmlMidiWriteByte(MidiData,0);             // Format = 1
  XmlMidiWriteByte(MidiData,1);             // Format = 1

  XmlMidiWriteByte(MidiData,0);             // Number of tracks
  XmlMidiWriteByte(MidiData,XmlStateData.MidiInstrumentMax+1);
  XmlMidiWriteByte(MidiData,1);           // Division
  XmlMidiWriteByte(MidiData,$E0);         // - per quarter note
  // Track header
  XmlMidiWriteByte(MidiData,ord('M'));
  XmlMidiWriteByte(MidiData,ord('T'));
  XmlMidiWriteByte(MidiData,ord('r'));
  XmlMidiWriteByte(MidiData,ord('k'));
  XmlStateData.TrackIndex:=MidiData.MidiDataIndexMax;  // Length
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$01);
  XmlMidiWriteByte(MidiData,$0);  // Instrument (overwrite of XmlMidiChangeTrack)
  // Name of song
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$08);
{$ifdef Darwin}
  XmlStateData.SongTitleCodePage:=XmlStateData.SongTitle;
{$else}
  XmlStateData.SongTitleCodePage:=UtfToCodepage(XmlStateData.SongTitle);
{$endif}
  XmlMidiWriteByte(MidiData,Length(XmlStateData.SongTitleCodePage));

  for i:=1 to Length(XmlStateData.SongTitleCodePage) do
    XmlMidiWriteByte(MidiData,ord(XmlStateData.SongTitleCodePage[i]));
  // FF 58 Time signature
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$58);
  XmlMidiWriteByte(MidiData,$04);
  XmlMidiWriteByte(MidiData,$06);
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$18);
  XmlMidiWriteByte(MidiData,$08);

  //// FF 59 Key Default TAS BORT?
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$59);
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$03);
  XmlMidiWriteByte(MidiData,$00);

  // FF 51 Tempo
  //// beregning?    
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$51);
  XmlMidiWriteByte(MidiData,$03);
  // Microseconds per quarter note = 500000. Default tempo = 120 per minute.
  // If the tune is swing, then for every two notes an extra timetick is added
  // to prolong the first note. Therefore: reduce by one third.
  if XmlStateData.Swing then
    i:=333333*XmlDefaultTempo div XmlStateData.Tempo
  else
    i:=500000*XmlDefaultTempo div XmlStateData.Tempo;
  XmlMidiWriteByte(MidiData,(i shr 16) and 255);
  XmlMidiWriteByte(MidiData,(i shr 8) and 255);
  XmlMidiWriteByte(MidiData,(i shr 0) and 255);


  for i:=0 to 15 do
    begin
    // C0 default instrument piano
    XmlMidiWriteByte(MidiData,$00);
    XmlMidiWriteByte(MidiData,$C0+i);
    XmlMidiWriteByte(MidiData,$0);
    end;

  // B0 default bank (0)
  XmlMidiWriteByte(MidiData,$00);
  XmlMidiWriteByte(MidiData,$B0);
  XmlMidiWriteByte(MidiData,$00);
  XmlMidiWriteByte(MidiData,$00);

  // Note?
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$B0);
  XmlMidiWriteByte(MidiData,$07);
  XmlMidiWriteByte(MidiData,$64);
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$0A);
  XmlMidiWriteByte(MidiData,$40);
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$5B);
  XmlMidiWriteByte(MidiData,$1E);
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$5D);
  XmlMidiWriteByte(MidiData,$1E);
  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlMidiChangeTrack
//
//     Purpose:     The produce the data to change track in the  midi data
//
//     Parameters:  XmlStateData = data read from Xml
//                  MidiData = produced data according to the midi standard
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure XmlMidiChangeTrack(var XmlStateData: TXmlStateData; var MidiData: TMidiData);

var
    i: integer;              // Loop through text (names)
    j: integer;              // Loop through Midi Instruments
    k: integer;              // Loop through Score Instruments
    TrackLength: integer;
    Found: boolean;

  begin
  XmlMidiWriteByte(MidiData,0);        // Write end of track
  XmlMidiWriteByte(MidiData,$FF);      // (command 2F)
  XmlMidiWriteByte(MidiData,$2F);
  XmlMidiWriteByte(MidiData,$00);

  // Write track length. Do not count the 32 bit value
  // (4 Bytes is part of track header)
  TrackLength:=MidiData.MidiDataIndexMax-XmlStateData.TrackIndex-4;
  MidiData.MidiData[XmlStateData.TrackIndex]:=(TrackLength shr 24) and $ff;
  MidiData.MidiData[XmlStateData.TrackIndex+1]:=(TrackLength shr 16) and $ff;
  MidiData.MidiData[XmlStateData.TrackIndex+2]:=(TrackLength shr 8) and $ff;
  MidiData.MidiData[XmlStateData.TrackIndex+3]:=(TrackLength shr 0) and $ff;

  MidiData.MidiDataSelected[XmlStateData.TrackIndex]:=(TrackLength shr 24) and $ff;
  MidiData.MidiDataSelected[XmlStateData.TrackIndex+1]:=(TrackLength shr 16) and $ff;
  MidiData.MidiDataSelected[XmlStateData.TrackIndex+2]:=(TrackLength shr 8) and $ff;
  MidiData.MidiDataSelected[XmlStateData.TrackIndex+3]:=(TrackLength shr 0) and $ff;

  XmlMidiWriteByte(MidiData,ord('M'));
  XmlMidiWriteByte(MidiData,ord('T'));
  XmlMidiWriteByte(MidiData,ord('r'));
  XmlMidiWriteByte(MidiData,ord('k'));
  XmlStateData.TrackIndex:=MidiData.MidiDataIndexMax;
  // The track data length is filled in later (At end of track)
  XmlMidiWriteByte(MidiData,$0);
  XmlMidiWriteByte(MidiData,$0);
  XmlMidiWriteByte(MidiData,$0);
  XmlMidiWriteByte(MidiData,$0);

  // Restart time counter (new notes from beginning)
  XmlStateData.NoteDelta:=0;

  // All settings depending on channel
  with XmlStateData do
    begin
    MidiChannelIndex:=XmlStateData.MidiChannel[XmlStateData.PartNumber];
  // Midi can have only 16 channels. Start all over. ////
    MidiChannelIndex:=MidiChannelIndex and 15;
{
  // Instrument - program number
    for i:=0 to MidiInstrumentMax-1 do
      begin
////      if XmlStateData.MidiInstrumentPart[i]=XmlStateData.PartNumber then
      if XmlStateData.MidiInstrumentPart[i]=XmlStateData.TrackNumber then
        begin
  XmlMidiWriteByte(MidiData,0);
        XmlMidiWriteByte(MidiData,$C0+XmlStateData.MidiInstrumentChannel[i]);
////        XmlMidiWriteByte(MidiData,XmlStateData.MidiInstrumentProgram[MidiChannelIndex]);

        XmlMidiWriteByte(MidiData,XmlStateData.MidiInstrumentProgram[XmlStateData.TrackNumber]);
        end;
      end;
}

       XmlMidiWriteByte(MidiData,0);
       XmlMidiWriteByte(MidiData,$C0+XmlStateData.MidiInstrumentChannel[XmlStateData.TrackNumber]);
       XmlMidiWriteByte(MidiData,XmlStateData.MidiInstrumentProgram[XmlStateData.TrackNumber]);

    // Part or Instrument name
{
    XmlMidiWriteByte(MidiData,$00);
    XmlMidiWriteByte(MidiData,$FF);
    XmlMidiWriteByte(MidiData,$03);
    XmlMidiWriteByte(MidiData,Length(XmlStateData.PartNames[XmlStateData.PartIndex]));
    for i:=1 to Length(XmlStateData.PartNames[XmlStateData.PartIndex]) do
    XmlMidiWriteByte(MidiData,ord(XmlStateData.PartNames[XmlStateData.PartIndex][i]));
}
    // Channel/Instrument name. Search the right one
    found:=false;
    for j:=0 to XmlStateData.MidiInstrumentMax-1 do
      begin
      if XmlStateData.MidiInstrumentPart[j]=XmlStateData.PartNumber then
      //// Konstigt - i PC-versionen finns denna rad - och det funkar
      //// men "Jag vet en dejlig rosa" visar fel text med fil från Finale
        begin
        XmlMidiWriteByte(MidiData,$00);
        XmlMidiWriteByte(MidiData,$FF);
        XmlMidiWriteByte(MidiData,$20);
        XmlMidiWriteByte(MidiData,$01);
        XmlMidiWriteByte(MidiData,XmlStateData.MidiInstrumentChannel[j] and 15);

        // Look up the corresponding instrument name in score-instrument part
        for k:=0 to XmlStateData.ScoreInstrumentMax-1 do
        if XmlStateData.MidiInstrument[j]=XmlStateData.ScoreInstrument[k] then
          begin
          found:=true;
          XmlMidiWriteByte(MidiData,$00);
          XmlMidiWriteByte(MidiData,$FF);
          XmlMidiWriteByte(MidiData,$03);
          if XmlStateData.MidiInstrumentChannel[j]<>9 then
            begin
            // Real different instruments
            XmlMidiWriteByte(MidiData,Length(XmlStateData.ScoreInstrumentNames[k]));
            for i:=1 to Length(XmlStateData.ScoreInstrumentNames[k]) do
            XmlMidiWriteByte(MidiData,ord(XmlStateData.ScoreInstrumentNames[k][i]));
            end
          else
            begin
            // Percussion channel 10
            XmlMidiWriteByte(MidiData,Length(XmlStateData.PartNames[XmlStateData.PartNumber]));
            for i:=1 to Length(XmlStateData.PartNames[XmlStateData.PartNumber]) do
            XmlMidiWriteByte(MidiData,ord(XmlStateData.PartNames[XmlStateData.PartNumber][i]));
            end;
          end;
        end;
      end;
    if not found then
      begin // No corresponding Midi Instrument declaration

      for k:=0 to XmlStateData.ScoreInstrumentMax-1 do
      if PartNumber=XmlStateData.ScoreInstrumentPart[k] then
        begin
        XmlMidiWriteByte(MidiData,$00);
        XmlMidiWriteByte(MidiData,$FF);
        XmlMidiWriteByte(MidiData,$20);
        XmlMidiWriteByte(MidiData,$01);
////        XmlMidiWriteByte(MidiData,XmlStateData.MidiInstrumentChannel[j] and 15);
        XmlMidiWriteByte(MidiData,k and 15);

        XmlMidiWriteByte(MidiData,$00);
        XmlMidiWriteByte(MidiData,$FF);
        XmlMidiWriteByte(MidiData,$03);

        XmlMidiWriteByte(MidiData,Length(XmlStateData.ScoreInstrumentNames[k]));
        for i:=1 to Length(XmlStateData.ScoreInstrumentNames[k]) do
        XmlMidiWriteByte(MidiData,ord(XmlStateData.ScoreInstrumentNames[k][i]));
        end;
      end;

  // Time signature
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$58);
  XmlMidiWriteByte(MidiData,$04);
    XmlMidiWriteByte(MidiData,XmlStateData.Beats);

  // Midi specifies the lower part of time signature in power of two
    XmlMidiWriteByte(MidiData,Log2(XmlStateData.BeatType));
  XmlMidiWriteByte(MidiData,$18);
  XmlMidiWriteByte(MidiData,$08);

  // Key signature ???? ////
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$59);
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$00);
  XmlMidiWriteByte(MidiData,$00);

  // Panorama
  XmlMidiWriteByte(MidiData,0);
    XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
  XmlMidiWriteByte(MidiData,$0A);
////  XmlMidiWriteByte(MidiData,XmlStateData.Pan[XmlStateData.PartNumber]);
  XmlMidiWriteByte(MidiData,XmlStateData.Pan[XmlStateData.TrackNumber]);
  //// Volume ????

  //// ????
  XmlMidiWriteByte(MidiData,$0);
    XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
  XmlMidiWriteByte(MidiData,$07);
  XmlMidiWriteByte(MidiData,$64);
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$5B);
  XmlMidiWriteByte(MidiData,$1E);
  XmlMidiWriteByte(MidiData,$02);
  XmlMidiWriteByte(MidiData,$5D);
  XmlMidiWriteByte(MidiData,$1E);
  end;
  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlMidiEnd
//
//     Purpose:     The produce the data to terminate midi data
//
//     Parameters:  XmlStateData = data read from Xml
//                  MidiData = produced data according to the midi standard
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure XmlMidiEnd(var XmlStateData: TXmlStateData; var MidiData: TMidiData);

var TrackLength: integer;

  begin
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$2F);
  XmlMidiWriteByte(MidiData,$00);

  // Write track length. Do not count the 32 bit value
  // (4 Bytes is part of track header)
  TrackLength:=MidiData.MidiDataIndexMax-XmlStateData.TrackIndex-4;
  MidiData.MidiData[XmlStateData.TrackIndex]:=(TrackLength shr 24) and $ff;
  MidiData.MidiData[XmlStateData.TrackIndex+1]:=(TrackLength shr 16) and $ff;
  MidiData.MidiData[XmlStateData.TrackIndex+2]:=(TrackLength shr 8) and $ff;
  MidiData.MidiData[XmlStateData.TrackIndex+3]:=(TrackLength shr 0) and $ff;

  MidiData.MidiDataSelected[XmlStateData.TrackIndex]:=(TrackLength shr 24) and $ff;
  MidiData.MidiDataSelected[XmlStateData.TrackIndex+1]:=(TrackLength shr 16) and $ff;
  MidiData.MidiDataSelected[XmlStateData.TrackIndex+2]:=(TrackLength shr 8) and $ff;
  MidiData.MidiDataSelected[XmlStateData.TrackIndex+3]:=(TrackLength shr 0) and $ff;
  end;


//---------------------------------------------------------------------------
//
//     Function:    NoteToXmlPitch
//
//     Purpose:     To convert the Midi data to XML note (C..B)
//                  (the midi note value (0..127 - Central C = 60)
//
//     Parameters:  NoteValue =  0..127
//
//     Returns:     The note value C,C+,D,D+,E,F,F+,G,G+,A,A+,B
//
//     Notes:       none
//
//---------------------------------------------------------------------------

function NoteToXmlPitch(NoteValue: integer): string;

var r: string;

  begin
  if StringToInt(XmlStateData.Fifths)>=0 then
    case (NoteValue) mod 12 of
    0: r:='C';
    1: r:='C+';
    2: r:='D';
    3: r:='D+';
    4: r:='E';
    5: r:='F';
    6: r:='F+';
    7: r:='G';
    8: r:='G+';
    9: r:='A';
    10: r:='A+';
    11: r:='B';
    end
  else
    case (NoteValue) mod 12 of
    0: r:='C';
    1: r:='D-';
    2: r:='D';
    3: r:='E-';
    4: r:='E';
    5: r:='F';
    6: r:='G-';
    7: r:='G';
    8: r:='A-';
    9: r:='A';
    10: r:='B-';
    11: r:='B';
    end;

  NoteToXmlPitch:=r;
  end;

//---------------------------------------------------------------------------
//
//     Procedure:   NoteInfoWrite
//
//     Purpose:     To produce midi data for the whole song. Data stored in
//                  Xml-time-divisions is transformed to DeltaTime (time to next
//                  event + note).
//
//     Parameters:  Mididata = structure and array containing
//                             notes for each xml-division, on
//                             or off etc.
//
//     Returns:     void (Data placed in public data by WriteMidiByte etc.)
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure NoteInfoWrite(var MidiData: TMidiData);

var i: integer;  // Loop through note data
    n: integer;  // Loop through lyrics for this note
    k: integer;  // Loop through syllable of lyrics
    p: int64;    // Position (time count) for previous note
    t: int64;  // For tempo calculation - value to MIDI
    InterNotepause: boolean;  // Extra pause between notes
    Vibrato: boolean;  // Current note has vibrato
    Scoop: boolean;  // Current note has scoop (pitch from low to tone)
    Plop: boolean;  // Current note has plop (pitch from high to tone)
    Doit: boolean;  // Current note has plop (pitch from tone to high)
    Falloff: boolean;  // Current note has plop (pitch from tone to low)
    Trill: boolean;  // Current note has trill
    WavyLine: boolean;  // Current note has trill
    Bend: boolean;    // A bend shall be applied
    delta: int64; // Computed pause before next midi event
    deltavibrato: int64; // Computed pause before next vibrato midi event
    deltafermataoff: int64; // Extra delta at fermata
    deltafermataon: int64; // Extra delta at fermata
    deltastaccato: int64;  ////
    deltaarpeggiate: int64; // For short notes of arpeggiate
    deltaarpeggiatetotal: int64; // For short notes of arpeggiate
    deltashortnotes: int64; // Steel some time for arpeggiata and grace
    deltashortnotestotal : int64; // Steel some time for arpeggiata and grace
    deltagrace: int64; // For short notes of arpeggiate
    deltagracetotal: int64; // For short notes of arpeggiate
    counttotal: int64;  // Arpeggiate or grace
    count: int64;       // Arpeggiate or grace total
    Arpeggiatecount: integer;
    gracecounttotal: integer;
    arpeggiatecounttotal: integer;
    levelvibrato: integer; // Level 0..200 converted to 0..127
    v: integer;  // Count vibratos
    Parm1: integer;   ////
    Parm2: integer;
    Parm3: integer;
{$ifdef FileDump}
    FileDump: Textfile;  //// TEMPORARY !!!! TEST !!!!
{$endif}
    DeltaFirstTime: integer; // For arpeggio and grace at first note/pause
    MicroToneAlter: integer;
    MicroToneAlterByte1: byte;
    MicroToneAlterByte2: byte;
    CurrentTempo: integer;
    CurrentMeasure: integer;

  // Help function for NoteInfoWrite
  procedure SetNumberOfArpeggiateAndGrace;      //// i skal være argument  !!!!

  var k: integer;

    begin
    // Arpeggiate and Grace plays notes with little timedifference
    arpeggiatecount:=0;
    arpeggiatecounttotal:=0;
    gracecounttotal:=0;
    deltashortnotes:=0;
    deltagrace:=0;
    if PNoteInfo^[i].Arpeggiate then          
      begin
      for k:=MidiNoteFirst to MidiNoteLast do if k in PNoteInfo^[i].NoteArpeggiate then inc(arpeggiatecounttotal);
      // The last note in Arpeggiate needs no delay
      dec(arpeggiatecounttotal);
      deltaarpeggiate:=MultFactorDefault div 16
      end;
    if PNoteInfo^[i].NoteGraceIndex-PNoteInfo^[i].NoteGraceIndexStart>0 then
      begin
      deltagrace:=MultFactorDefault div 16;
      gracecounttotal:=PNoteInfo^[i].NoteGraceIndex-PNoteInfo^[i].NoteGraceIndexStart;
      end;

    if Arpeggiatecounttotal>gracecounttotal then counttotal:=Arpeggiatecounttotal
    else counttotal:=gracecounttotal;
    Arpeggiatecount:=0;
    if counttotal>0 then
      begin
      deltagrace:=MultFactorDefault div 16;
      // Time to be stolen from the delta
      deltashortnotes:=deltagrace*(counttotal);
      end;
    count:=0;
    end;

  // Help function for NoteInfoWrite
  procedure FreeGraceNotes;

  var
      m: integer;
      gracecount: integer;

    begin
    // More grace notes than arpeggiates?    ////
    // Any grace notes?
    if (PNoteInfo^[i].NoteGraceIndexStart>0) and (gracecounttotal>arpeggiatecounttotal) then
    with XmlStateData do
      begin
      for gracecount:=0 to gracecounttotal-arpeggiatecounttotal-1 do
        begin
          begin
          if (gracecount=0) and
           (delta+deltafirsttime+deltashortnotestotal+deltastaccato>=deltagrace)
          then
            begin    // Reduce first delta and start first note
            delta:=delta+deltafirsttime+deltashortnotestotal+deltastaccato;
            deltashortnotestotal:=0;
            deltafirsttime:=0;
            deltastaccato:=0;

            delta:=delta-deltagrace*(gracecounttotal-gracecount);

            if (deltastaccato>0) then
            if (delta<0) and (deltastaccato>0) then
              begin
              if delta+deltastaccato>0 then
                begin
                delta:=delta+deltastaccato;
                deltastaccato:=0;
                end;
              end;

            if delta<0 then     //// TEST !!!!
              delta:=0;

            for m:=MidiNoteFirst to MidiNoteLast do   //// Max note !!!!!
            if m in NoteInfoGrace[PNoteInfo^[i].NoteGraceIndexStart+gracecount].NoteOn then
              begin
              if delta<0 then
                begin
                delta:=0;
                XmlError('Internal Error: '+'Delta<0');
                end;
              XmlMidiWriteValue(MidiData,delta);
              XmlMidiWriteByte(MidiData,$90+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,m);
              XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
              delta:=0;
              end;
            end
          else if (gracecount>0) and (gracecount<gracecounttotal) then
            begin    // Next is just a short delta. Stop and start
            delta:=deltagrace;
            for m:=MidiNoteFirst to MidiNoteLast do
            if m in NoteInfoGrace[PNoteInfo^[i].NoteGraceIndexStart+gracecount-1].NoteOn then
              begin // Stop previous notes
              XmlMidiWriteValue(MidiData,delta);
              XmlMidiWriteByte(MidiData,$80+MidiChannelIndex);
                XmlMidiWriteByte(MidiData,m);
              XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
              delta:=0;
              end;
            for m:=MidiNoteFirst to MidiNoteLast do
            if m in NoteInfoGrace[PNoteInfo^[i].NoteGraceIndexStart+gracecount].NoteOn then
              begin // Start new ones
              XmlMidiWriteValue(MidiData,0);
              XmlMidiWriteByte(MidiData,$90+MidiChannelIndex);
                XmlMidiWriteByte(MidiData,m);
              XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
              end
            end;
          end;
        end;
        delta:=deltagrace;
        for m:=MidiNoteFirst to MidiNoteLast do
        if m in NoteInfoGrace[PNoteInfo^[i].NoteGraceIndexStart+gracecounttotal-1].NoteOn then
          begin // Stop last note
          XmlMidiWriteValue(MidiData,delta);
          XmlMidiWriteByte(MidiData,$80+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,m);
          XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
        delta:=0;
        p:=i;
        end
      end;
    end;

  // Help function for NoteInfoWrite
  procedure HandleOnNote(n: integer);

  var m: integer;

    begin
      // Arpeggiate or Grace notes and room to play them?
      if arpeggiatecounttotal<>0 then
        begin
        if (count=0) and (count<counttotal)
        then
          begin
          inc(arpeggiatecount);
          inc(count);
          if gracecounttotal>arpeggiatecounttotal then
            delta:=0  // Delta already taken
          else
            begin    // Time taken from this note
            if deltashortnotestotal=0 then
              delta:=delta-deltaarpeggiate*counttotal
            else
              begin // Time stolen from NoteOff
              delta:=delta+deltashortnotestotal-deltaarpeggiate*counttotal;
              deltashortnotestotal:=0
              end;
            end;
          // Any lost grace note missing?     //// Forkert beregning !!!!
          if (gracecounttotal>0) and (arpeggiatecounttotal>0) then
            begin
            for m:=MidiNoteFirst to MidiNoteLast do   //// Max note !!!!!
            if m in NoteInfoGrace[PNoteInfo^[i].NoteGraceIndexStart+arpeggiatecounttotal+count].NoteOn then
              begin
              XmlMidiWriteValue(MidiData,delta);
              XmlMidiWriteByte(MidiData,$90+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,m);
              XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
              delta:=0;
              end;
            end;
          end
        else if (count>0) and (count<counttotal) then
          begin
          delta:=deltaarpeggiate;
          inc(count);
          // Now generate stop and start the arpeggiate notes, first stop,
          // then start and last stop last note
          for m:=MidiNoteFirst to MidiNoteLast do
          if m in NoteInfoGrace[PNoteInfo^[i].NoteGraceIndexStart+arpeggiatecounttotal+count-1].NoteOn then
            begin // Stop previous notes. Only first note uses the delta
            XmlMidiWriteValue(MidiData,delta);
            XmlMidiWriteByte(MidiData,$80+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,m);
            XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
            delta:=0;
            end;
          for m:=MidiNoteFirst to MidiNoteLast do
          if m in NoteInfoGrace[PNoteInfo^[i].NoteGraceIndexStart+arpeggiatecounttotal+count].NoteOn then
            begin // Start new ones. No delta.
            XmlMidiWriteValue(MidiData,0);
            XmlMidiWriteByte(MidiData,$90+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,m);
            XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
            end
          end

        else if (counttotal>0) and (count=counttotal) then
          begin
          delta:=deltaarpeggiate;
          for m:=MidiNoteFirst to MidiNoteLast do
          if m in NoteInfoGrace[PNoteInfo^[i].NoteGraceIndexStart+arpeggiatecounttotal+count-1].NoteOn then
            begin // Stop previous notes
            XmlMidiWriteValue(MidiData,delta);
            XmlMidiWriteByte(MidiData,$80+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,m);
            XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
            delta:=0;
            end;
          counttotal:=0;
          arpeggiatecounttotal:=0;
          gracecounttotal:=0;
          end;
        end;

    // Tempo Change? Only in First Part
////    if XmlStateData.PartNumber=0 then
      begin
    if (PNoteInfo^[i].Tempo>0) and (PNoteInfo^[i].Tempo<>CurrentTempo) then
      begin
      CurrentTempo:=PNoteInfo^[i].Tempo;
      // FF 51 Tempo
      //// beregning?
      XmlMidiWriteByte(MidiData,0);
      XmlMidiWriteByte(MidiData,$FF);
      XmlMidiWriteByte(MidiData,$51);
      XmlMidiWriteByte(MidiData,$03);
      if XmlStateData.Swing then
      t:=333333*XmlDefaultTempo div CurrentTempo //// ????
      else
        t:=500000*XmlDefaultTempo div CurrentTempo; //// ????
      XmlMidiWriteByte(MidiData,(t shr 16) and 255);
      XmlMidiWriteByte(MidiData,(t shr 8) and 255);
      XmlMidiWriteByte(MidiData,(t shr 0) and 255);
      end;
      if (PNoteInfo^[i].MeasureNumber>0) and (PNoteInfo^[i].MeasureNumber<>CurrentMeasure) then
        begin
        CurrentMeasure:=PNoteInfo^[i].MeasureNumber;
        // Use meta-event cue point to indicate new measure
        XmlMidiWriteByte(MidiData,0);
        XmlMidiWriteByte(MidiData,$FF);
        XmlMidiWriteByte(MidiData,$07);
        XmlMidiWriteByte(MidiData,$03);
        t:=CurrentMeasure;
        XmlMidiWriteByte(MidiData,(t shr 16) and 255);
        XmlMidiWriteByte(MidiData,(t shr 8) and 255);
        XmlMidiWriteByte(MidiData,(t shr 0) and 255);
        end;
      end;


      if InterNotePause then
        begin
        XmlMidiWriteValue(MidiData,delta+InterNotePauseLength);
        InterNotePause:=false;
        end
      else
        begin
        XmlMidiWriteValue(MidiData,delta);
        end;

    // Works only correct if only one note at a time (per part)
    // Adjust tone pitch specified in the <alter>-command
    if XmlStateData.MicroTones then
      begin
      // The Midi-standard specifies $2000 as no adjustment
      MicroToneAlter:=(PNoteInfo^[i].MicroToneAlter)+$2000;
      MicroToneAlterByte1:=MicroToneAlter mod 128;
      MicroToneAlterByte2:=MicroToneAlter div 128;

      XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
      XmlMidiWriteByte(MidiData,MicroToneAlterByte1);
      XmlMidiWriteByte(MidiData,MicroToneAlterByte2);
      XmlMidiWriteByte(MidiData,0);
      end;

      XmlMidiWriteByte(MidiData,$90+XmlStateData.MidiChannelIndex);
      XmlMidiWriteByte(MidiData,n);
      XmlMidiWriteByte(MidiData,PNoteInfo^[i].Volume);
      delta:=0;
      p:=i;
      end;

  // procedure NoteInfoWrite
  begin
  Vibrato:=false;
  Plop:=false;
  Scoop:=false;
  Doit:=false;
  Falloff:=false;
  Trill:=false;
  WavyLine:=false;
  Bend:=false;
  CurrentTempo:=XmlDefaultTempo;
  CurrentMeasure:=0;
  deltafermataon:=0;
  deltastaccato:=0;
  deltafermataoff:=0;
  deltaarpeggiate:=0;
  deltaarpeggiatetotal:=0;
  deltagrace:=0;
  deltagracetotal:=0;
  deltashortnotes:=0;
  deltashortnotestotal:=0;
  deltafirsttime:=MultFactorDefault;

////  Parm1:=0;      //// Vibrato parms
////  Parm2:=0;
  p:=0;
  InterNotePause:=false;
  // Time signature        (OBS: to gange ////)
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$58);
  XmlMidiWriteByte(MidiData,$04);
  XmlMidiWriteByte(MidiData,XmlStateData.Beats);
  //// square root ???? eller power of two?
  XmlMidiWriteByte(MidiData,Log2(XmlStateData.BeatType));
  XmlMidiWriteByte(MidiData,$18);
  XmlMidiWriteByte(MidiData,$08);
  // FF 59 Key
  XmlMidiWriteByte(MidiData,0);
  XmlMidiWriteByte(MidiData,$FF);
  XmlMidiWriteByte(MidiData,$59);
  XmlMidiWriteByte(MidiData,$02);
  // Fifths
  if StringToInt(XmlStateData.Fifths)>=0 then
    XmlMidiWriteByte(MidiData,(StringToInt(XmlStateData.Fifths)) and 255)
  else
    XmlMidiWriteByte(MidiData,256-(StringToInt(XmlStateData.Fifths) and 255));
  XmlMidiWriteByte(MidiData,$00);

{$define zzzzFileDump}
{$ifdef FileDump}
  //// TEST !!!!
  if MidiTest then
    begin
    AssignFile(FileDump,FileName+'-Track'+IntToStr(XmlStateData.TrackNumber+1)+'.txt');
    rewrite(FileDump);
  {
      writeln(FileDump,'--------------');
      for k:=0 to 15 do
      for m:=0 to 7 do
        begin
        i:=k+m*16;
        s:=IntToStr(i)+':'+IntToHex(i,2)+':'+NoteToXmlPitch(i);
        while length(s)<10 do s:=s+' ';
        write(FileDump,s);
        if m=7 then writeln(FileDump);
        end;
      writeln(FileDump,'--------------');
  }
    for i:=0 to NumberOfTimeSamples-1 do
      begin
      s:='';
      f:=false;
      for n:=MidiNoteFirst to MidiNoteLast do
        begin
        if n in PNoteInfo^[i].NoteContinue then
          begin
          if f then s:=s+' ='+NoteToXmlPitch(n) else s:='='+NoteToXmlPitch(n);
          f:=true;
          end
        else if n in PNoteInfo^[i].NoteOff then
          begin
          if f then s:=s+' -'+NoteToXmlPitch(n) else s:='-'+NoteToXmlPitch(n);
          f:=true;
          end
        else if n in PNoteInfo^[i].NoteOn then
          begin
          if f then s:=s+' +'+NoteToXmlPitch(n) else s:='+'+NoteToXmlPitch(n);
          f:=true;
          end
        end;
      s:=IntToStr(i)+':'+s;
      if f then writeln(FileDump,s);
      end;
    CloseFile(FileDump);
    end;
{$endif}

  // NoteOff - stop notes before start new ones
  XmlStateData.XmlTimeIndexMax:=XmlStateData.XmlTimeIndex+
  XmlStateData.CommonMultiplicator*XmlStateData.BeatsMax;
  for i:=0 to XmlStateData.XmlTimeIndexMax do
    begin
    if PNoteInfo^[i].Fermata then
      begin
      deltafermataoff:=deltafermataoff+MultFactorDefault;
      deltafermataon:=deltafermataon+MultFactorDefault div 2;
      end;

    if PNoteInfo^[i].FermataMissing>0 then
      begin
////      deltafermataon:=deltafermataon+MultFactorDefault;
////      deltafermataoff:=deltafermataoff+MultFactorDefault div 2;
      deltafirsttime:=deltafirsttime+
      (MultFactorDefault+MultFactorDefault div 2)*PNoteInfo^[i].FermataMissing;
      end;

    SetNumberOfArpeggiateAndGrace;

    if (PNoteInfo^[i].NoteOff<>[]) or (PNoteInfo^[i].NoteContinue<>[]) then
    with XmlStateData do
      begin // Stop notes
      delta:=(MultFactorDefault*(i-p) div XmlStateData.CommonMultiplicator);
      if PNoteInfo^[i].Staccato then
        begin
        deltastaccato:=delta div 2;
        delta:=delta-deltastaccato;
        end;
      delta:=delta+deltafermataoff+ {deltastaccato+}
                deltaarpeggiatetotal+deltagracetotal;
      // Steel a little time for arpeggiato and grace notes
      if (deltashortnotes>0) and (delta>deltashortnotes) then
        begin
        delta:=delta-deltashortnotes;
        deltashortnotestotal:=deltashortnotestotal+deltashortnotes;
        deltashortnotes:=0;
        end;
      deltafermataoff:=0;
      deltaarpeggiatetotal:=0;
      deltagracetotal:=0;
      delta:=delta+deltafirsttime;
      deltafirsttime:=0;


      for n:=MidiNoteFirst to MidiNoteLast do
        begin ////
        if (n in PNoteInfo^[i].NoteOff) or
           ((n in PNoteInfo^[i].NoteContinue) and (not (n in PNoteInfo^[i].NoteOn))) then
          begin
          if PNoteInfo^[i].NoteChannelOff[n]>0 then
          XmlStateData.MidiChannelIndex:=PNoteInfo^[i].NoteChannelOff[n]-1;

          // Vibrato
          if Vibrato and (delta>InterNotePauseLength+2*PNoteInfo^[i].AttributeParm1+1) then
          with XmlStateData do
            begin
            deltavibrato:=delta div (8*Parm1+1);
            // Eight step vibrato - number of times specified in Parm1
            levelvibrato:=(Parm2*127) div 800;
            if levelvibrato>31 then levelvibrato:=31
            else if levelvibrato<0 then levelvibrato:=0;
{$define NewVibrato}
{$ifdef NewVibrato}
            levelvibrato:=levelvibrato div 2;
            for v:=1 to Parm1 do
              begin
              delta:=delta-8*deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato);      // 1
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((levelvibrato) and 127));
              XmlMidiWriteValue(MidiData,deltavibrato);      // 2
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((2*levelvibrato) and 127));
              XmlMidiWriteValue(MidiData,deltavibrato);      // 3
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((3*levelvibrato) and 127));
              XmlMidiWriteValue(MidiData,deltavibrato);      // 4
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((4*levelvibrato) and 127));
              XmlMidiWriteValue(MidiData,deltavibrato);      // 5
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((3*levelvibrato) and 127));
              XmlMidiWriteValue(MidiData,deltavibrato);      // 6
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((2*levelvibrato) and 127));
              XmlMidiWriteValue(MidiData,deltavibrato);      // 7
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((levelvibrato) and 127));
              XmlMidiWriteValue(MidiData,deltavibrato);      // 8
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((0*levelvibrato) and 127));
              end;

{$else}
            for v:=1 to Parm1 do
              begin
              delta:=delta-8*deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,1);
              XmlMidiWriteByte(MidiData,levelvibrato);
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,1);
              XmlMidiWriteByte(MidiData,levelvibrato*2);
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,1);
              XmlMidiWriteByte(MidiData,levelvibrato*3);
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,1);
              XmlMidiWriteByte(MidiData,levelvibrato*4);
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,1);
              XmlMidiWriteByte(MidiData,levelvibrato*3);
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,1);
              XmlMidiWriteByte(MidiData,levelvibrato*2);
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,1);
              XmlMidiWriteByte(MidiData,levelvibrato);
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,1);
              XmlMidiWriteByte(MidiData,0);
              end;
            XmlMidiWriteValue(MidiData,delta);
            XmlMidiWriteByte(MidiData,$B0+MidiChannelIndex);
            XmlMidiWriteByte(MidiData,1);
            XmlMidiWriteByte(MidiData,0);
{$endif}
          Vibrato:=false;
            end

          // Scoop
          else if Scoop and (delta>InterNotePauseLength+2*32+1) then
          with XmlStateData do
            begin
            Parm1:=32;
            deltavibrato:=delta div Parm1;
            // raise the pitch value slow - number of times specified in Parm1
            levelvibrato:=2;


            XmlMidiWriteValue(MidiData,0);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,0);
            for v:=1 to Parm1-2 do
              begin
              delta:=delta-deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,(levelvibrato*v) and 127);
              end;
            XmlMidiWriteValue(MidiData,delta);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            Scoop:=false;
            end

          // Plop
          else if Plop and (delta>InterNotePauseLength+2*32+1) then
          with XmlStateData do
            begin
            Parm1:=32;
            deltavibrato:=delta div Parm1;
            // raise the pitch value slow - number of times specified in Parm1
            levelvibrato:=2;


            XmlMidiWriteValue(MidiData,0);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            for v:=1 to Parm1-2 do
              begin
              delta:=delta-deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40-((levelvibrato*v) and 127));
              end;
            XmlMidiWriteValue(MidiData,delta);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            Plop:=false;
            end

          // Doit
          else if Doit and (delta>InterNotePauseLength+2*32+1) then
          with XmlStateData do
            begin
            Parm1:=32;
            deltavibrato:=delta div Parm1;
            // raise the pitch value slow - number of times specified in Parm1
            levelvibrato:=2;
            XmlMidiWriteValue(MidiData,0);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            for v:=1 to Parm1-2 do
              begin
              delta:=delta-deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((levelvibrato*v) and 127));
              end;
            XmlMidiWriteValue(MidiData,delta);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            Doit:=false;
            end

          // Falloff
          else if Falloff and (delta>InterNotePauseLength+2*32+1) then
          with XmlStateData do
            begin
            Parm1:=32;
            deltavibrato:=delta div Parm1;
            // raise the pitch value slow - number of times specified in Parm1
            levelvibrato:=2;
            XmlMidiWriteValue(MidiData,0);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            for v:=1 to Parm1-2 do
              begin
              delta:=delta-deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40-((levelvibrato*v) and 127));
              end;
            XmlMidiWriteValue(MidiData,delta);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            Falloff:=false;
            end

          // Bend (Must have a parm value > 0 - We want parm1>10)
          else if Bend and (delta>InterNotePauseLength+2*32+1) then
          with XmlStateData do
            begin
            if Parm1<10 then Parm1:=10;
            // Let 2/3 be increase and 1/1 be the reached tone
            deltavibrato:=(2*delta) div (3*Parm1);
            // raise the pitch value slow - number of times specified in Parm1
            levelvibrato:=1;
            XmlMidiWriteValue(MidiData,0);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            for v:=1 to Parm1 do
              begin
              delta:=delta-deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
              XmlMidiWriteByte(MidiData,0);
              XmlMidiWriteByte(MidiData,$40+((levelvibrato*v) and 127));
              end;
            XmlMidiWriteValue(MidiData,delta);
            XmlMidiWriteByte(MidiData,$E0+XmlStateData.MidiChannelIndex);
            XmlMidiWriteByte(MidiData,0);
            XmlMidiWriteByte(MidiData,$40);
            Bend:=false;
            end

          // Trill
          else if Trill and (delta>2*InterNotePauseLength+2*11+1) then //// Calculation?
          with XmlStateData do
            begin
            if MidiChannelIndex=9 then Parm3:=12 else Parm3:=4;
////            deltavibrato:=delta div (Parm3*2+1);  //// Define 4?
            // Let one third end the trill.
            deltavibrato:=delta div (Parm3*3+1);  //// Define 4?
            // Two step trill
            for v:=1 to Parm3 do //// Define 4?
              begin
              delta:=delta-2*deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato); ////-InterNotePauseLength);
              XmlMidiWriteByte(MidiData,$80+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,Parm1);
              XmlMidiWriteByte(MidiData,Parm2);

              XmlMidiWriteValue(MidiData,0);////InterNotePauseLength);
              XmlMidiWriteByte(MidiData,$90+MidiChannelIndex);
              if MidiChannelIndex=9 then
              XmlMidiWriteByte(MidiData,Parm1)
              else
              XmlMidiWriteByte(MidiData,Parm1+1);
              XmlMidiWriteByte(MidiData,Parm2);

              XmlMidiWriteValue(MidiData,deltavibrato); ////-InterNotePauseLength);
              XmlMidiWriteByte(MidiData,$80+MidiChannelIndex);
              if MidiChannelIndex=9 then
              XmlMidiWriteByte(MidiData,Parm1)
              else
              XmlMidiWriteByte(MidiData,Parm1+1);
              XmlMidiWriteByte(MidiData,Parm2);

              XmlMidiWriteValue(MidiData,0); ////InterNotePauseLength);
              XmlMidiWriteByte(MidiData,$90+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,Parm1);
              XmlMidiWriteByte(MidiData,Parm2);
              end;

            Trill:=false;
            end


          // WavyLine
          else if WavyLine and (delta>2*InterNotePauseLength+2*11+1) then //// Calculation?
          with XmlStateData do
            begin
            if MidiChannelIndex=9 then Parm3:=12 else Parm3:=4;
            deltavibrato:=delta div (Parm3*2+1);  //// Define 4?
            // Two step trill
            for v:=1 to Parm3 do //// Define 4?
              begin
              delta:=delta-2*deltavibrato;
              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$80+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,Parm1);
              XmlMidiWriteByte(MidiData,Parm2);

              XmlMidiWriteValue(MidiData,0);
              XmlMidiWriteByte(MidiData,$90+MidiChannelIndex);
              if MidiChannelIndex=9 then
              XmlMidiWriteByte(MidiData,Parm1)
              else
              XmlMidiWriteByte(MidiData,Parm1+1);
              XmlMidiWriteByte(MidiData,Parm2);

              XmlMidiWriteValue(MidiData,deltavibrato);
              XmlMidiWriteByte(MidiData,$80+MidiChannelIndex);
              if MidiChannelIndex=9 then
              XmlMidiWriteByte(MidiData,Parm1)
              else
              XmlMidiWriteByte(MidiData,Parm1+1);
              XmlMidiWriteByte(MidiData,Parm2);

              XmlMidiWriteValue(MidiData,0);
              XmlMidiWriteByte(MidiData,$90+MidiChannelIndex);
              XmlMidiWriteByte(MidiData,Parm1);
              XmlMidiWriteByte(MidiData,Parm2);
              end;
            WavyLine:=false;
            end;

          if InterNotePause or (delta<=InterNotePauseLength) then
            begin
            XmlMidiWriteValue(MidiData,delta);
            end
          else
            begin
            InterNotePause:=true;
            XmlMidiWriteValue(MidiData,delta-InterNotePauseLength);
            end;
          XmlMidiWriteByte(MidiData,$80+MidiChannelIndex);
          XmlMidiWriteByte(MidiData,n);
          XmlMidiWriteByte(MidiData,XmlStateData.Volume[XmlStateData.TrackNumber]);
          delta:=0;
          p:=i;
          end;
        end;
      end;

    // Start new notes
    if PNoteInfo^[i].NoteOn<>[] then
    with XmlStateData do
      begin // Start notes
      delta:=(MultFactorDefault*(i-p) div XmlStateData.CommonMultiplicator);
      // Add also unused deltafermataoff
      delta:=delta+deltafermataon+deltafermataoff;////+deltastaccato;
      deltafermataon:=0;
      deltafermataoff:=0;
      delta:=delta+deltafirsttime;
      deltafirsttime:=0;
      if counttotal>0 then FreeGraceNotes;

      // Delta staccato may be used by grace notes. If note do it here
      delta:=delta+deltastaccato;
      deltastaccato:=0;

      arpeggiatecount:=0;

      // Take arpeggio notes first
      for n:=MidiNoteFirst to MidiNoteLast do
        begin
        if PNoteInfo^[i].NoteChannelOn[n]>0 then
        XmlStateData.MidiChannelIndex:=PNoteInfo^[i].NoteChannelOn[n]-1;
        if (n in PNoteInfo^[i].NoteArpeggiate) and (not (n in PNoteInfo^[i].NoteContinue)) then
        HandleOnNote(n);
            end;

      for n:=MidiNoteFirst to MidiNoteLast do
            begin
        if PNoteInfo^[i].NoteChannelOn[n]>0 then
        XmlStateData.MidiChannelIndex:=PNoteInfo^[i].NoteChannelOn[n]-1;
        if (n in PNoteInfo^[i].NoteOn) and (not (n in PNoteInfo^[i].NoteContinue))
          and (not (n in PNoteInfo^[i].NoteArpeggiate)) then
          begin
        HandleOnNote(n);
          Parm1:=n; // Save for possible trill
          Parm2:=PNoteInfo^[i].Volume;
          end;
        end;

      if Length(PNoteInfo^[i].SongText)>0 then
        begin
        // Write the text (bytes from the lyric)
        XmlMidiWriteByte(MidiData,0);
        XmlMidiWriteByte(MidiData,$FF);
        XmlMidiWriteByte(MidiData,$05);
        // XML is UTF8 - convert it to codepage data (for Delphi)
        // For Lazarus the function UtfToCodepage is a dummy function
        XmlStateData.LyricCodepage:=UtfToCodepage(PNoteInfo^[i].SongText);
        XmlMidiWriteByte(MidiData,Length(XmlStateData.LyricCodepage));
        for k:=1 to Length(XmlStateData.LyricCodePage) do
          XmlMidiWriteByte(MidiData,ord(XmlStateData.LyricCodepage[k]));
        end;

      if PNoteInfo^[i].Attribute=AttributeVibrato then
        begin
        Vibrato:=true;
        Parm1:=PNoteInfo^[i].AttributeParm1;
        Parm2:=PNoteInfo^[i].AttributeParm2;
        end
      else if PNoteInfo^[i].Attribute=AttributeTrill then
        begin
        Trill:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeWavyLine then
        begin
        WavyLine:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeTurn then
        begin
        Trill:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeDelayedTurn then
        begin
        Trill:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeInvertedTurn then
        begin
        Trill:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributePlop then
        begin
        Plop:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeScoop then
        begin
        Scoop:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeVibrato then
        begin
        Vibrato:=true;
        Parm1:=PNoteInfo^[i].AttributeParm1;
        Parm2:=PNoteInfo^[i].AttributeParm2;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeDoit then
        begin
        Doit:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeFalloff then
        begin
        Falloff:=true;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end
      else if PNoteInfo^[i].Attribute=AttributeBend then
        begin
        Bend:=true;
        Parm1:=PNoteInfo^[i].AttributeParm1;
        Parm2:=PNoteInfo^[i].AttributeParm2;
        PNoteInfo^[i].Attribute:=AttributeNone;
        end;
      end;
    end;
  end;


//---------------------------------------------------------------------------
//
//     Function:    NoteToMidiPitch
//
//     Purpose:     To convert the Xml data (note value, octave and alter) to
//                  the midi note value (0..127 - Central C = 60)
//
//     Parameters:  NoteValue =  (Letter C,D,E,F,G,A,B)
//                  NoteOctave = number of octave    //// more info
//                  NoteAlter = Number of half steps added
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

function NoteToMidiPitch(NoteValue: string; NoteOctave: string; NoteAlter: string): integer;

var r: integer;

  begin
  r:=0;
  if (Length(NoteValue)=1) and (NoteValue[1] in ['A'..'G']) and
     (Length(NoteOctave)=1) and (NoteOctave[1] in ['0'..'9']) then
    begin
      case NoteValue[1] of
      'C': r:=0;
      'D': r:=2;
      'E': r:=4;
      'F': r:=5;
      'G': r:=7;
      'A': r:=9;
      'B': r:=11;
      end;
    r:=r+(StringToInt(NoteOctave)+1)*12;
    r:=r+StringToInt(NoteAlter);
    end
  else
    begin
    if NoteValue='' then
      XmlError(LinguaTextNoteValue+NoteValue+'/'+NoteOctave+'Pitch is missing')
    else
      XmlError(LinguaTextNoteValue+NoteValue+'/'+NoteOctave+'Pitch is outside range');
    end;
  NoteToMidiPitch:=r;
  end;



//---------------------------------------------------------------------------
//
//     Function:    GraceNoteMerge
//
//     Purpose:     There may be more than one set of grace notes
//                  If the new one is longer that the previous then
//                  merge into new array. Otherwise merge into old array
//
//     Parameters:  TimeIndex = Time tick ////
//
//     Returns:     void
//
//     Notes:       none           //// How many notes may be played?
//
//---------------------------------------------------------------------------

procedure GraceNoteMerge(TimeIndex: int64);

var i: integer;

  begin
  if (NoteInfoGraceIndex-NoteInfoGraceIndexStart>0) and
     (PNoteInfo^[TimeIndex].NoteGraceIndex>0) then
    begin
    // More than last time?
      if NoteInfoGraceIndex-NoteInfoGraceIndexStart>
          PNoteInfo^[TimeIndex].NoteGraceIndex-
          PNoteInfo^[TimeIndex].NoteGraceIndexStart then
        begin // Yes more notes, merge into new array
        for i:=1 to PNoteInfo^[TimeIndex].NoteGraceIndex-
                    PNoteInfo^[TimeIndex].NoteGraceIndexStart do
          begin
          NoteInfoGrace[NoteInfoGraceIndex-i].NoteOn:=
               NoteInfoGrace[NoteInfoGraceIndex-i].NoteOn+
             NoteInfoGrace[PNoteInfo^[TimeIndex].NoteGraceIndex-i].NoteOn;
          // Use new array
          PNoteInfo^[TimeIndex].NoteGraceIndex:=NoteInfoGraceIndex;
          PNoteInfo^[TimeIndex].NoteGraceIndexStart:=NoteInfoGraceIndexStart;
          end
        end
      else
        begin // No, merge into old array. Indexes are already right
        for i:=1 to NoteInfoGraceIndex-NoteInfoGraceIndexStart do
          begin
          NoteInfoGrace[PNoteInfo^[TimeIndex].NoteGraceIndex-i].NoteOn:=
               NoteInfoGrace[NoteInfoGraceIndex-i].NoteOn+
             NoteInfoGrace[PNoteInfo^[TimeIndex].NoteGraceIndex-i].NoteOn;
          end;
        end;
      end
  else if NoteInfoGraceIndex-NoteInfoGraceIndexStart>0 then
    begin
    PNoteInfo^[TimeIndex].NoteGraceIndex:=NoteInfoGraceIndex;
    PNoteInfo^[TimeIndex].NoteGraceIndexStart:=NoteInfoGraceIndexStart;
    end;

  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlMidiNoteOn
//
//     Purpose:     To start a new note.
//
//     Parameters:  XmlStateData = State containing data read from the Xml-file
//                  MidiData = structure for the Midi data read
//                  NotePitch = The note value (hight) according to midi standard
//
//     Returns:     void
//
//     Notes:       The note value (height) comes from the Xml data (XmlStateData)
//
//---------------------------------------------------------------------------

procedure XmlMidiNoteOn(var XmlStateData: TXmlStateData);

var NotePitch: integer;  // Note height according to Midi (convert from XML)
    NoteMicrotoneAlter: integer; // Small note tone adjustment
    i: integer;          // Loop through song text
    DropNote: boolean;      // Used if the note is already there and longer
    Index: int64;        // Time index locally
    IndexOff: int64;     // Time index locally for staccato

  begin
  DropNote:=false;

  if (XmlStateData.PartNumber<0) or (XmlStateData.PartNumber>64) then
    XmlError(LinguaTextPartNumberIsOutOfRange+IntToStr(XmlStateData.PartNumber)+')');

  with XmlStateData do
    begin
    NotePitch:=NoteToMidiPitch(XmlStateData.Step,
                          XmlStateData.Octave,XmlStateData.Alter)+Transpose;

    NoteMicrotoneAlter:=AlterStringToInt(XmlStateData.Alter);
    if NoteMicrotoneAlter>=$800 then
      begin // If more than half a tone up, then adjust NotePitch
      NoteMicrotoneAlter:=NoteMicrotoneAlter-$1000;
      inc(NotePitch);
      end;
    if NoteMicrotoneAlter<=-$800 then
      begin // If more than half a tone down, then adjust NotePitch
      NoteMicrotoneAlter:=NoteMicrotoneAlter+$1000;
      dec(NotePitch);
      end;

    end;

  Assert(StringToInt(XmlStateData.ActualNotes)>0,'NormalNotes = 0 in '+FileName);
  XmlStateData.Tie[XmlStateData.PartNumber,NotePitch]:=XmlStateData.TieStart;

  // If too many time divisions (Notes * divisions) then stop. Program must be recompiled

  if XmlStateData.XmlTimeIndex+XmlStateData.Duration>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName);
{
  else
    begin
  Assert(
    XmlStateData.XmlTimeIndex+XmlStateData.Duration<NumberOfTimeSamples,
    'Too many time divisions (notes) in '+FileName);
  }
  // More than one note may noted to start at the same time.
  // Find the end and keep only the longest
  if NotePitch in PNoteInfo^[XmlStateData.XmlTimeIndex].NoteOn then
    begin
    // Search noteoff (To decide which is longist and selet this)
    i:=0;
    while (XmlStateData.XmlTimeIndex+i<MaxNumberOfTimeDivision) and
          (not (NotePitch in PNoteInfo^[XmlStateData.XmlTimeIndex+i].NoteOff)) do
      begin
      inc(i);
      end;
    if  (XmlStateData.XmlTimeIndex+i<MaxNumberOfTimeDivision) and
        (NotePitch in PNoteInfo^[XmlStateData.XmlTimeIndex+i].NoteOff) then
      if i<XmlStateData.Duration then
        begin
        // Next note is longer - move Note Off to the longer note instead.
        PNoteInfo^[XmlStateData.XmlTimeIndex+i].NoteOff:=
                    PNoteInfo^[XmlStateData.XmlTimeIndex+i].NoteOff-[NotePitch];
////        PNoteInfo^[XmlStateData.XmlTimeIndex+i].Divisions:=XmlStateData.Divisions;
////        PNoteInfo^[XmlStateData.XmlTimeIndex+i].NoteChannel[NotePitch]:=XmlStateData.MidiChannelIndex;
        end
      else
        begin
        // The note is shorter - drop it.
        DropNote:=true;
        end;
    end;

  // Write to the TNoteInfo record structure
  if DropNote then
    begin
    PNoteInfo^[XmlStateData.XmlTimeIndex].SongText:=
      PNoteInfo^[XmlStateData.XmlTimeIndex].SongText+XmlStateData.Lyric;
      XmlStateData.Lyric:='';

    XmlStateData.XmlTimeIndexlast:=XmlStateData.XmlTimeIndex;
    if not XmlStateData.Chord then
      begin
      XmlStateData.XmlTimeIndex:=XmlStateData.XmlTimeIndex+XmlStateData.Duration;



  if XmlStateData.XmlTimeIndex>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName);   ////



      if XmlStateData.XmlTimeIndex>XmlStateData.XmlTimeIndexMaxInMeasure then
        XmlStateData.XmlTimeIndexMaxInMeasure:=XmlStateData.XmlTimeIndex;
    end
    end
  else if not XmlStateData.Chord then
    begin // Normal note (may be first note in chord)
    PNoteInfo^[XmlStateData.XmlTimeIndex].Attribute:=XmlStateData.Attribute;
    PNoteInfo^[XmlStateData.XmlTimeIndex].AttributeParm1:=XmlStateData.AttributeParm1;
    PNoteInfo^[XmlStateData.XmlTimeIndex].AttributeParm2:=XmlStateData.AttributeParm2;

    // Add the note to the set of notes
      PNoteInfo^[XmlStateData.XmlTimeIndex].NoteOn:=
             PNoteInfo^[XmlStateData.XmlTimeIndex].NoteOn+[NotePitch];
    PNoteInfo^[XmlStateData.XmlTimeIndex].MicrotoneAlter:=NoteMicrotoneAlter;

    // Slow down 20 percent for ritardando
    if (XmlStateData.Ritardando>0) then
    if (XmlStateData.Ritardando>XmlStateData.Duration) then
      begin
      XmlStateData.Tempo:=XmlStateData.Tempo-((XmlStateData.Tempo*XmlStateData.Duration) div
                            (XmlStateData.Divisions*5));
      if XmlStateData.Ritardando>XmlStateData.Duration then
        XmlStateData.Ritardando:=XmlStateData.Ritardando-XmlStateData.Duration
      else
        XmlStateData.Ritardando:=0;
      end;

    PNoteInfo^[XmlStateData.XmlTimeIndex].Tempo:=XmlStateData.Tempo;

    // Save the channel number to handle different instruments
    PNoteInfo^[XmlStateData.XmlTimeIndex].NoteChannelOn[NotePitch]:=XmlStateData.MidiChannelIndex+1;
    if XmlStateData.Arpeggiate then
      begin
      PNoteInfo^[XmlStateData.XmlTimeIndex].Arpeggiate:=XmlStateData.Arpeggiate;
      PNoteInfo^[XmlStateData.XmlTimeIndex].NoteArpeggiate:=
             PNoteInfo^[XmlStateData.XmlTimeIndex].NoteArpeggiate+[NotePitch];
      XmlStateData.Arpeggiate:=false;
      end;

    GraceNoteMerge(XmlStateData.XmlTimeIndex);

    ////
      XmlStateData.NoteOn[XmlStateData.PartNumber,NotePitch]:=true;

    if XmlStateData.TieStart then
      begin
      Index:=XmlStateData.XmlTimeIndex+XmlStateData.Duration;
      PNoteInfo^[Index].NoteContinue:=PNoteInfo^[Index].NoteContinue+[NotePitch];
      PNoteInfo^[Index].NoteChannelOff[NotePitch]:=XmlStateData.MidiChannelIndex+1;
      end
    else
      begin
        IndexOff:=XmlStateData.XmlTimeIndex+XmlStateData.Duration;
      Index:=XmlStateData.XmlTimeIndex+XmlStateData.Duration;
      PNoteInfo^[IndexOff].NoteOff:=PNoteInfo^[IndexOff].NoteOff+[NotePitch];
      PNoteInfo^[Index].NoteChannelOff[NotePitch]:=XmlStateData.MidiChannelIndex+1;
      end;

    // If more notes on same timeindex, then use only one text
    if PNoteInfo^[XmlStateData.XmlTimeIndex].SongText='' then
    PNoteInfo^[XmlStateData.XmlTimeIndex].SongText:=XmlStateData.Lyric;
    XmlStateData.Lyric:='';

    XmlStateData.XmlTimeIndexlast:=XmlStateData.XmlTimeIndex;
    XmlStateData.XmlTimeIndex:=XmlStateData.XmlTimeIndex+XmlStateData.Duration;



  if XmlStateData.XmlTimeIndex>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName);   ////



    if XmlStateData.XmlTimeIndex>XmlStateData.XmlTimeIndexMaxInMeasure then
      XmlStateData.XmlTimeIndexMaxInMeasure:=XmlStateData.XmlTimeIndex;
    end
  else
    begin // Chord. Place note at same time division (i.e. XmlTimeIndexLast)
    Index:=XmlStateData.XmlTimeIndexLast;
      begin
      PNoteInfo^[Index].NoteOn:=PNoteInfo^[Index].NoteOn+[NotePitch];
      PNoteInfo^[Index].NoteChannelOn[NotePitch]:=XmlStateData.MidiChannelIndex+1;

      if XmlStateData.Arpeggiate then
        begin
        PNoteInfo^[Index].Arpeggiate:=XmlStateData.Arpeggiate;
        PNoteInfo^[Index].NoteArpeggiate:=
             PNoteInfo^[Index].NoteArpeggiate+[NotePitch];
        XmlStateData.Arpeggiate:=false;
        end;
      end;

    GraceNoteMerge(Index);

    if not XmlStateData.TieStart then
      begin
      Index:=XmlStateData.XmlTimeIndexLast+XmlStateData.Duration;
      PNoteInfo^[Index].NoteOff:=PNoteInfo^[Index].NoteOff+[NotePitch];
      PNoteInfo^[Index].NoteChannelOff[NotePitch]:=XmlStateData.MidiChannelIndex+1;
      end
    else
      begin
      Index:=XmlStateData.XmlTimeIndexLast+XmlStateData.Duration;
      PNoteInfo^[Index].NoteContinue:=PNoteInfo^[Index].NoteContinue+[NotePitch];
{$define UseChordChange}
{$ifdef UseChordChange}
      PNoteInfo^[Index].NoteChannelOff[NotePitch]:=XmlStateData.MidiChannelIndex+1;
 {$endif}
      end;
    PNoteInfo^[XmlStateData.XmlTimeIndex].SongText:=XmlStateData.Lyric;
    XmlStateData.Lyric:='';

    end;
  if XmlStateData.Fermata then
    begin
      PNoteInfo^[XmlStateData.XmlTimeIndex].Fermata:=true;
    XmlStateData.Fermata:=false;
    end;
    if XmlStateData.FermataMissing>0 then
    begin
      PNoteInfo^[XmlStateData.XmlTimeIndexLast].FermataMissing:=
        PNoteInfo^[XmlStateData.XmlTimeIndexLast].FermataMissing+
        XmlStateData.FermataMissing;
      XmlStateData.FermataMissing:=0;
    end;
  if XmlStateData.Staccato then
      PNoteInfo^[XmlStateData.XmlTimeIndex].Staccato:=true;
  PNoteInfo^[XmlStateData.XmlTimeIndex].MeasureNumber:=XmlStateData.MeasureNumber;
  XmlStateData.Attribute:=AttributeNone;
  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlMidiNotePause
//
//     Purpose:     To produce a midi pause. Do it by increasing the time delta
//                  (before next midi event)
//
//     Parameters:  XmlStateData = State containing data read from the Xml-file
//                  MidiData = structure for the Midi data read
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure XmlMidiNotePause(var XmlStateData: TXmlStateData);

  begin
  if XmlStateData.Fermata then
    begin
    inc(XmlStateData.FermataMissing);
    end;

{$define zzzzNewFarmatahandling}
{$ifdef NewFarmatahandling}
  if XmlStateData.FermataMissing>0 then
  begin
    PNoteInfo^[XmlStateData.XmlTimeIndexLast].FermataMissing:=
      PNoteInfo^[XmlStateData.XmlTimeIndexLast].FermataMissing+
      XmlStateData.FermataMissing;
    XmlStateData.FermataMissing:=0;
  end;
{$endif}

{$define zzzzOldFarmatahandling}
{$ifdef OldFarmatahandling}
  if XmlStateData.Fermata then
    begin
      PNoteInfo^[XmlStateData.XmlTimeIndex].Fermata:=true;
    XmlStateData.Fermata:=false;
    end;
{$endif}

  ///// Same limit for tracks and parts (parts becomes midi tracks)   ????
  if (XmlStateData.PartNumber<0) or (XmlStateData.PartNumber>Tracks) then
    XmlError(LinguaTextPartNumberIsOutOfRange+IntToStr(XmlStateData.PartNumber)+')');
  //// both error and assert?
  Assert(StringToInt(XmlStateData.NormalNotes)>0,'NormalNotes = 0 in '+FileName);
  if not XmlStateData.Chord then
    begin
////    XmlStateData.XmlTimeIndexlast:=XmlStateData.XmlTimeIndex;
    XmlStateData.XmlTimeIndex:=XmlStateData.XmlTimeIndex+XmlStateData.Duration;



  if XmlStateData.XmlTimeIndex>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName);   ////



    if XmlStateData.XmlTimeIndex>XmlStateData.XmlTimeIndexMaxInMeasure then
      XmlStateData.XmlTimeIndexMaxInMeasure:=XmlStateData.XmlTimeIndex;
    end;
  end;

//---------------------------------------------------------------------------
//
//     Function:    XmlMidiNoNote
//
//     Purpose:     To produce a midi pause, but instead of a note
//                  (When more instruments)
//                  Do it by increasing the time delta
//                  (before next midi event)
//
//     Parameters:  XmlStateData = State containing data read from the Xml-file
//                  MidiData = structure for the Midi data read
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure XmlMidiNoNote(var XmlStateData: TXmlStateData);

  begin
  if XmlStateData.Fermata then
    begin
    PNoteInfo^[XmlStateData.XmlTimeIndex].Fermata:=true;
    XmlStateData.Fermata:=false;
    end;
  ///// Same limit for tracks and parts (parts becomes midi tracks)   ????
  if (XmlStateData.PartNumber<0) or (XmlStateData.PartNumber>Tracks) then
    XmlError(LinguaTextPartNumberIsOutOfRange+IntToStr(XmlStateData.PartNumber)+')');
  //// both error and assert?
  Assert(StringToInt(XmlStateData.NormalNotes)>0,'NormalNotes = 0 in '+FileName);
  if not XmlStateData.Chord then
    begin
    XmlStateData.XmlTimeIndexlast:=XmlStateData.XmlTimeIndex;
    XmlStateData.XmlTimeIndex:=XmlStateData.XmlTimeIndex+XmlStateData.Duration;



  if XmlStateData.XmlTimeIndex>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName);   ////



    if XmlStateData.XmlTimeIndex>XmlStateData.XmlTimeIndexMaxInMeasure then
      XmlStateData.XmlTimeIndexMaxInMeasure:=XmlStateData.XmlTimeIndex;
    end;
  end;


//---------------------------------------------------------------------------
//
//     Function:    XmlMidiNoteOff
//
//     Purpose:     To produce the midi event Note Off.
//
//     Parameters:  XmlStateData = State containing data read from the Xml-file
//                  MidiData = structure for the Midi data read
//                  NotePitch = The note value (hight) according to midi standard
//
//     Returns:     void
//
//     Notes:       none
//
//---------------------------------------------------------------------------

procedure XmlMidiNoteOff(var XmlStateData: TXmlStateData;
                         NotePitch: integer);

  begin
  if (XmlStateData.PartNumber<0) or (XmlStateData.PartNumber>64) then
    XmlError(LinguaTextPartNumberIsOutOfRange+IntToStr(XmlStateData.PartNumber)+')');

  if not XmlStateData.Tie[XmlStateData.PartNumber,NotePitch] then
    begin
    XmlStateData.NoteOn[XmlStateData.PartNumber,NotePitch]:=false;
    end
  end;

//---------------------------------------------------------------------------
//
//     Function:    ComputeTimeSignature
//
//     Purpose:     The time signature may consist of "+" and strange
//                  notation. Try to do this
//
//     Parameters:  none
//
//     Returns:     void
//
//     Notes:       XmlStateData global data structure
//
//---------------------------------------------------------------------------

procedure ComputeTimeSignature;

var i: integer;
   max: integer;
   NoZeroes: boolean;

  begin
  with XmlStateData do
    begin
    NoZeroes:=true;
    for i:=1 to NoOfTimeSignatures do if BeatTypeList[i]=0 then NoZeroes:=false;

    if (NoOfTimeSignatures>0) and NoZeroes then
      begin
      // Normalise to let all denominators be the max value
      max:=1;
      for i:=1 to NoOfTimeSignatures do
      if XmlStateData.BeatTypeList[i]>max then max:=BeatTypeList[i]; ////
      for i:=1 to NoOfTimeSignatures do
        begin
        XmlStateData.BeatsList[i]:=(BeatsList[i]*max) div BeatTypeList[i];
        XmlStateData.BeatTypeList[i]:=max;
        end;
      // Compute the sum of all intervals
      Beats:=0;
      for i:=1 to NoOfTimeSignatures do Beats:=Beats+BeatsList[i];
      BeatType:=max;
      end
    else
      begin
      Beats:=4;
      BeatType:=4;
      end;
    NoOfTimeSignatures:=0;
    end;
  end;

//---------------------------------------------------------------------------
//
//     Function:    AddToInt
//
//     Purpose:     Add some numbers in a string - return integer
//
//     Parameters:  s = A string containing one or more integers separated
//                  by "+"
//
//     Returns:     An integer result
//
//     Notes:       none
//
//---------------------------------------------------------------------------

function AddToInt(s: string): integer;

var n: integer; // The final number
    p: integer; // Position of '+'

  begin
  n:=0;
    repeat
    p:=Pos('+',s);
    if p>0 then
      begin
      n:=n+StringToInt(copy(s,1,p-1));
      s:=copy(s,p+1,Length(s)-p);
      end;
    until p=0;
  n:=n+StringToInt(s);
  AddToInt:=n;
  end;
////
type TStringPart = (BeforeNumber, InNumber, AfterNumber, ErrorNumber); ////


function IsNumber(s: string): boolean;

var i: integer; // Index to string
    r: TStringPart;

  begin
  r:=BeforeNumber;
  for i:=1 to Length(s) do
    case r of
    BeforeNumber: if s[i] in ['0'..'9'] then r:=InNumber;
    InNumber: if not (s[i] in ['0'..'9']) then r:=AfterNumber;
    AfterNumber: if s[i] in ['0'..'9'] then r:=ErrorNumber;
    end;
  IsNumber:=r=AfterNumber;
  end;


//---------------------------------------------------------------------------
//
//     Function:    PreReadXmlToMidiFile
//
//     Purpose:     Preprocess the MusicXml file to find number of verses
//                  and the tempo - this must be known before reading the
//                  file for real processing and also estimate the time signature
//
//     Parameters:  FileName = the MusicXml file name
//
//     Returns:     void
//
//     Notes:       FileName will normally be the global file name, except when
//                  a file is zipped. Then the extension is .mxl but changed to .xml
//
//---------------------------------------------------------------------------

procedure PreReadXmlToMidiFile(FileName: string);

// To minimise memory usage an attempt to find the biggist common divisor
// (for duration versus time divisor) is done. Only these 25 primes are tested.

const
  MaxDurations=10000;
  MaxPrime=24;
  Prime: array[0..MaxPrime] of integer=
    (2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97);

var Symbol: TSymbol;  // Symbol read
    v: integer;       // Verse
    Counts: integer;      // Time
    MaxCounts: integer;
    Beats: integer;
    Measure: int64;     // Measure number used to build array of Segno, Dacapo etc.
    i: integer;
    j: integer;
    k: integer;
    p: integer; // Used for position of string in text
    s: string;  // Used to extract text at position
    AllDurations: array[1..MaxDurations] of boolean;
    D: integer; // Temporary divisor
    IsDivisor: boolean;
    DirectionState: TDirectionState;

  begin
  DirectionState:=DirectionStateNone;
  XmlStateData.NoteNumber:=0;
  Measure:=1;
  
  xmllinenumber:=0;
  DataIn:=0;
  XmlStateData.DurationListIndex:=0;
{$ifdef FPC}
{$ifdef Darwin}
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(FileName,fmOpenRead or fmShareDenyNone);
{$else}
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(FileName,fmOpenRead or fmShareDenyNone);
////    DataIn:=FileOpen(PChar(UTF8ToAnsi(FileName)),fmOpenRead or fmShareDenyNone)
    //// Vi kommer inte her
////  else if (FileName<>'') and (FileExists(FileName)) then
////      DataIn:=FileOpen(PChar(FileName),fmOpenRead or fmShareDenyNone);
{$endif}
{$else}
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(PChar(FileName),fmOpenRead or fmShareDenyNone);
{$endif}
  XmlState:=XmlStateIdentifyUtf;
  DataInCount:=1;  // Set "Not end of file"

  XmlStateData.MaxVerseNumberPart:=0;
  XmlStateData.MaxVerseNumberAll:=0;
  XmlStateData.PartNumber:=0;
  XmlStateData.TrackNumber:=0;
  XmlStateData.Tempo:=XmlDefaultTempo;
  XmlStateData.TempoOrg:=XmlDefaultTempo;
  XmlStateData.Ritardando:=0;

  Counts:=0;
  MaxCounts:=0;
  XmlStateData.MaxMeasure:=1;
  XmlStateData.Repeats:=false;

  ReadXmlBufferIndex:=0;
  XmlDataInCount:=0;
  XmlStateData.Divisions:=1;
  XmlStateData.CommonDivisor:=1;
  XmlStateData.BeatsMax:=1;
  XmlStateData.Swing:=false;
  XmlStateData.VoltaMax:=0;

  for i:=1 to MaxDurations do
    AllDurations[i]:=false;

  for i:=0 to MaxMeasures do
    MeasureDirections[i]:=[];

  // Do the preprocessing
    repeat
    Symbol:=NextSymbol;

    if Symbol.StateSymbol=XmlStateDirectionBegin then
      begin
      DirectionState:=DirectionStateBegin;
      end
    else if Symbol.StateSymbol=XmlStateDirectionTypeBegin then
      begin
      if DirectionState=DirectionStateBegin then
        DirectionState:=DirectionStateTypeBegin;
      end
    else if Symbol.StateSymbol=XmlStateWordsBegin then
      begin
      if DirectionState=DirectionStateTypeBegin then
        DirectionState:=DirectionStateWordsBegin;
      end
    else if Symbol.StateSymbol=XmlStateWordsEnd then
      begin
      if Symbol.InfoBefore='To Coda' then
        MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionTocoda]
      else if Symbol.InfoBefore='Fine' then
        MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionFine]
      else if Symbol.InfoBefore='D.S.' then
        MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDalSegno]
      else if Symbol.InfoBefore='D.C.' then
        MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDacapo]
      else if Symbol.InfoBefore='D.C. al Fine' then
        MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDacapoAlFine]
      else if Symbol.InfoBefore='D.S. al Fine' then
        MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDalSegnoAlFine]
      else if Symbol.InfoBefore='D.S. al Coda' then
        MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDalSegnoAlCoda]
      else if Symbol.InfoBefore='D.C. al Coda' then
        MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDacapoAlCoda]
      else if lowercase(Symbol.InfoBefore)='swing' then
        XmlStateData.Swing:=true;

      if DirectionState=DirectionStateWordsBegin then
        DirectionState:=DirectionStateTypeBegin;
      end
    else if Symbol.StateSymbol=XmlStateDirectionTypeEnd then
      begin
      if DirectionState=DirectionStateTypeBegin then
        DirectionState:=DirectionStateBegin;
      end
    else if Symbol.StateSymbol=XmlStateDirectionEnd then
      begin
      if DirectionState=DirectionStateBegin then
        DirectionState:=DirectionStateNone;
      end
    else if (Symbol.StateSymbol=XmlStateSound) and (Symbol.InfoTempo<>'') then
      begin
      if ((Length(XmlStateData.VoltaText)=0) or
        (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaText)>0))
        then
      XmlStateData.Tempo:=StringToInt(Symbol.InfoTempo);
      XmlStateData.TempoOrg:=XmlStateData.Tempo;
      XmlStateData.Ritardando:=0;
      end
    else if Symbol.StateSymbol=XmlStateScorePartBegin then
      begin
      ////
      end
    else if Symbol.StateSymbol=XmlStatePartBegin then
      begin
      inc(XmlStateData.PartNumber);
////      XmlStateData.VoltaMax:=0;
      XmlStateData.VerseNumberMapLast[XmlStateData.PartNumber]:=0;
      XmlStateData.VerseNumberMapFirst[XmlStateData.PartNumber]:=MaxInt;
      end
    else if Symbol.StateSymbol=XmlStateDivisionsEnd then
      begin
      XmlStateData.Divisions:=StringToInt(Symbol.InfoBefore);
      end
    else if Symbol.StateSymbol=XmlStateBeatsEnd then
      begin
      Beats:=StringToInt(Symbol.InfoBefore);
      if Beats>XmlStateData.BeatsMax then
        XmlStateData.BeatsMax:=Beats;
      // Start division (to be used for arpeggio and grace notes)
      end
    else if Symbol.StateSymbol=XmlStateMeasureBegin then
      begin
      if IsNumber(Symbol.InfoNumber) then Measure:=StringToInt(Symbol.InfoNumber);
      if Measure>XmlStateData.MaxMeasure then XmlStateData.MaxMeasure:=Measure;
      if Counts>MaxCounts then MaxCounts:=Counts;
      Counts:=0;
      XmlStateData.RepeatInfoMeasure.RepeatStart:=GetFilePos(DataIn);
      XmlStateData.RepeatInfoMeasure.RepeatStartLineNumber:=XmlLineNumber;
      XmlStateData.RepeatInfoMeasure.RepeatMeasureNumber:=Measure;
      XmlStateData.RepeatInfoMeasure.RepeatStartState:=XmlStateMeasureBegin;
      XmlStateData.RepeatInfoMeasure.RepeatBeginOpen:=false;
      XmlStateData.RepeatInfoMeasure.RepeatNoteNumber:=XmlStateData.NoteNumber;
      XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
      end
    else if Symbol.StateSymbol=XmlStateDurationEnd then
      begin
      // Save durations > 0 and find biggest common divisor
      if StringToInt(Symbol.Infobefore)>0 then
        begin
        D:=(XmlStateData.CommonMultiplicator div XmlStateData.Divisions)*StringToInt(Symbol.Infobefore);
      AllDurations[D]:=true;

      //// MaxCount bruges vist ikke !!!!
      if XmlState=XmlStateBackupBegin then
        Counts:=Counts-StringToInt(Symbol.Infobefore)
      else if XmlState=XmlStatePitchBegin then
        Counts:=Counts+StringToInt(Symbol.Infobefore)
      else if XmlState=XmlStateForwardBegin then
        Counts:=Counts+StringToInt(Symbol.Infobefore);
      if Counts>MaxCounts then
        MaxCounts:=Counts;
          end;
      Symbol.StateSymbol:=XmlStateDurationEnd;
      end
    else if Symbol.StateSymbol=XmlStateBackupBegin then
      begin
      XmlState:=XmlStateBackupBegin;
      end
    else if Symbol.StateSymbol=XmlStateChord then
      begin
      XmlState:=XmlStateChord;
      end
    else if Symbol.StateSymbol=XmlStateForwardBegin then
      begin
      if XmlState<>XmlStateChord then
      XmlState:=XmlStateForwardBegin;
      end
    else if Symbol.StateSymbol=XmlStatePitchBegin then
      begin
      if XmlState<>XmlStateChord then
      XmlState:=XmlStatePitchBegin;
      end
    else if Symbol.StateSymbol=XmlStateNoteBegin then
    with XmlStateData do
      begin
      inc(XmlStateData.NoteNumber);
      XmlState:=XmlStateNoteBegin;
      end

    else if Symbol.StateSymbol=XmlStateRepeat then
    with XmlStateData do
      begin
      Repeats:=true;
      end
    else if Symbol.StateSymbol=XmlStateFermata then
      begin
      FermatInMeasureAll[Measure]:=true;
      end

    else if Symbol.StateSymbol=XmlStateFermataBegin then
      begin
      FermatInMeasureAll[Measure]:=true;
      end

    else if Symbol.StateSymbol=XmlStateAlterEnd then
      begin
      XmlStateData.Alter:=Symbol.InfoBefore;
      p:=pos('.',XmlStateData.Alter);
      s:=copy(XmlStateData.Alter,p+1,Length(XmlStateData.Alter)-p);
      // Is it a MicroTone and is this a quarter?
      if p>0 then
        XmlStateData.MicroTones:=true;
      end

    else if Symbol.StateSymbol=XmlStateEnding then
      begin
      if Symbol.InfoTypeType='"discontinue"' then
      VoltaDiscontinueInMeasure[Measure]:=true;
      for i:=1 to 10 do
        begin
        if (Pos(IntToStr(i),Symbol.InfoNumber)>0) then
        if XmlStateData.VoltaMax<i then
           XmlStateData.VoltaMax:=i;
        end;
      end
    else if Symbol.StateSymbol=XmlStateSegno then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionSegno];
      end
    else if Symbol.StateSymbol=XmlStateCoda then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionCoda];
      // Next note is the number after jump to Coda.
      CodaNoteNumber:=XmlStateData.NoteNumber+1;
      XmlStateData.RepeatInfoForwardCoda:=XmlStateData.RepeatInfoMeasure;
      end
    else if Symbol.StateSymbol=XmlStateFine then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionFine];
      end
    else if Symbol.StateSymbol=XmlStateDacapo then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDacapo];
      end
    else if Symbol.StateSymbol=XmlStateDacapoAlFine then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDacapoAlFine];
      end
    else if Symbol.StateSymbol=XmlStateDalsegnoAlFine then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDalsegnoAlFine];
      end
    else if Symbol.StateSymbol=XmlStateTocoda then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionTocoda];
      end
    else if Symbol.StateSymbol=XmlStateDalsegno then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDalSegno];
      end
    else if Symbol.StateSymbol=XmlStateDalSegnoAlCoda then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDalSegnoAlCoda];
      end
    else if Symbol.StateSymbol=XmlStateDacapoAlCoda then
      begin
      MeasureDirections[Measure]:=MeasureDirections[Measure]+[DirectionDacapoAlCoda];
      end

    else if Symbol.StateSymbol=XmlStateLyricBegin then
    with XmlStateData do
      begin
      v:=StringToInt(Symbol.InfoNumber);
      if v>XmlStateData.MaxVerseNumberPart then
      XmlStateData.MaxVerseNumberPart:=v;

      if v>VerseNumberMapLast[PartNumber] then VerseNumberMapLast[PartNumber]:=v;
      if v<VerseNumberMapFirst[PartNumber] then VerseNumberMapFirst[PartNumber]:=v;
      end
    until (Symbol.StateSymbol=XmlStateEnd) or (DataInCount<=0);

  FileClose(DataIn);


  // The current scheme (i.e. MusicXml-definition) for Lyrics can not handle
  // both verse numbers and repeat-numbers.
  // If repeats and two verses then assume one verse.
////  if (XmlStateData.Repeats) and (XmlStateData.MaxVerseNumberPart=2) then XmlStateData.MaxVerseNumberPart:=1;

  // Find a common divisor. But only if there is only one <division>-definition
  // Start by making a array of durations
  AllDurations[XmlStateData.CommonMultiplicator]:=true;
  if XmlStateData.DivisionCount>0 then
    begin
    XmlStateData.DurationListIndex:=0;
  for i:=1 to MaxDurations do
    begin
    if AllDurations[i] then
      begin
        XmlStateData.DurationList[XmlStateData.DurationListIndex]:=i;
        inc(XmlStateData.DurationListIndex);
      end;
    end;
  i:=0;
    // Find greatest common divisor - check only 25 first primes
  XmlStateData.CommonDivisor:=1;
  while i<25 do
    begin
    IsDivisor:=true;
      for j:=0 to XmlStateData.DurationListIndex-1 do
      begin
        if XmlStateData.DurationList[j] mod Prime[i]<>0 then
        begin
        IsDivisor:=false;
        end;
      end;
    if IsDivisor then
      begin
        for k:=0 to XmlStateData.DurationListIndex-1 do
          XmlStateData.DurationList[k]:=XmlStateData.DurationList[k] div Prime[i];
      XmlStateData.CommonDivisor:=XmlStateData.CommonDivisor*Prime[i];
      end
    else
      begin
      inc(i);
      end;
    end;
    // Reduce the multiplicator with the greatest common divisor (Save timesamples)
    XmlStateData.CommonMultiplicator:=XmlStateData.CommonMultiplicator div XmlStateData.CommonDivisor;
    XmlStateData.Divisions:=XmlStateData.CommonMultiplicator;
    MaxCounts:=MaxCounts div XmlStateData.CommonDivisor;
    end;

  // Estimate the time signature (in case the time signature is missing)
  XmlStateData.Beats:=4;
  XmlStateData.BeatType:=4;                                     //// DEF !!!!
  if (XmlStateData.Divisions>0) and
     (MaxCounts>=XmlStateData.Divisions) and
     ((MaxCounts div XmlStateData.Divisions)<100) then
    begin
    XmlStateData.Beats:=MaxCounts div XmlStateData.Divisions;
    end;
////  Assert(XmlStateData.MaxIndex<NumberOfTimeSamples,LinguaTextTooManyNotes);
  // Room for extra space for measure 0 and another in the end. Plus one for grace
////  XmlStateData.MaxIndex:=XmlStateData.DivisionsMax*(XmlStateData.MaxMeasure+3)*XmlStateData.BeatsMax;
//// NOTE: HVOR MANGE VERS OG GENTAGELSER O.S.V. ?
  XmlStateData.MaxIndex:=NumberOfTimeSamples;
  end;

//---------------------------------------------------------------------------
//
//     Function:    PreReadXmlDivisions
//
//     Purpose:     Preprocess the MusicXml file to find the
//                  <divisions>-definitions in the file
//
//     Parameters:  FileName = the MusicXml file name
//
//     Returns:     void
//
//     Notes:       This is Pass 1 of MusicXml reading
//                  (Pass 2 = PreReadXmlToMidiFile, process <duration>
//                  (Pass 3 = ReadXmlToMidiFile, real processing - create Midi
//---------------------------------------------------------------------------

procedure PreReadXmlDivisions(FileName: string);

// To minimise memory usage an attempt to find the biggist common divisor
// (for duration versus time divisor) is done. Only these 25 primes are tested.

const
  MaxPrime=24;
  Prime: array[0..MaxPrime] of integer=
    (2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97);
  DurationListMax=200;

var Symbol: TSymbol;  // Symbol read
    v: integer;       // Verse
    Counts: integer;      // Time
    MaxCounts: integer;   ////
    Beats: integer;
    Measure: int64;
    i: integer;
    j: integer;
    m: integer;
    DivisionList: array[0..DurationListMax] of integer;
    Multiplicator: integer;

  begin
  DataIn:=0;
  Measure:=0;
  XmlStateData.DivisionListIndex:=0;
{$ifdef FPC}
{$ifdef Darwin}
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(FileName,fmOpenRead or fmShareDenyNone);
{$else}
if (FileName<>'') and (FileExists(FileName)) then
////    DataIn:=FileOpen(PChar(UTF8ToAnsi(FileName)),fmOpenRead or fmShareDenyNone);
    DataIn:=FileOpen(FileName,fmOpenRead or fmShareDenyNone);
{$endif}
{$else}
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(PChar(FileName),fmOpenRead or fmShareDenyNone);
{$endif}
  XmlState:=XmlStateIdentifyUtf;
  DataInCount:=1;  // Set "Not end of file"

  XmlStateData.MaxVerseNumberPart:=0;
  XmlStateData.MaxVerseNumberAll:=0;
  XmlStateData.PartNumber:=0;
  XmlStateData.PartNumberMax:=0;
  XmlStateData.TrackNumber:=0;
  XmlStateData.Tempo:=XmlDefaultTempo;
  XmlStateData.TempoOrg:=XmlStateData.Tempo;
  XmlStateData.Ritardando:=0;

  Counts:=0;
  MaxCounts:=0;
  XmlStateData.MaxMeasure:=1;
  ReadXmlBufferIndex:=0;
  XmlDataInCount:=0;
  XmlStateData.Divisions:=1;
  XmlStateData.CommonDivisor:=1;
  XmlStateData.BeatsMax:=1;
  XmlStateData.DivisionCount:=0;

  // Do the preprocessing
    repeat
    Symbol:=NextSymbol;
    if (Symbol.StateSymbol=XmlStateSound) and (Symbol.InfoTempo<>'') then
      begin
      if ((Length(XmlStateData.VoltaText)=0) or
        (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaText)>0))
        then
      XmlStateData.Tempo:=StringToInt(Symbol.InfoTempo);
      end
    else if Symbol.StateSymbol=XmlStateScorePartBegin then
      begin
      XmlStateData.PartMap[XmlStateData.PartNumberMax]:=Symbol.InfoId;
      inc(XmlStateData.PartNumberMax);
      end
    else if Symbol.StateSymbol=XmlStatePartBegin then
      begin
      inc(XmlStateData.PartNumber);
      XmlStateData.VerseNumberMapLast[XmlStateData.PartNumber]:=0;
      XmlStateData.VerseNumberMapFirst[XmlStateData.PartNumber]:=MaxInt;
      end
    else if Symbol.StateSymbol=XmlStateDivisionsEnd then
      begin
      if XmlStateData.Divisions<>StringToInt(Symbol.InfoBefore) then
        begin
        inc(XmlStateData.DivisionCount);
        DivisionList[XmlStateData.DivisionListIndex]:=StringToInt(Symbol.InfoBefore);
        inc(XmlStateData.DivisionListIndex);
        end;
      XmlStateData.Divisions:=StringToInt(Symbol.InfoBefore);
      end
    else if Symbol.StateSymbol=XmlStateBeatsEnd then
      begin
      Beats:=StringToInt(Symbol.InfoBefore);
      if Beats>XmlStateData.BeatsMax then
        XmlStateData.BeatsMax:=Beats;
      // Start division (to be used for arpeggio and grace notes)
      end
    else if Symbol.StateSymbol=XmlStateMeasureBegin then
      begin
      if IsNumber(Symbol.InfoNumber) then Measure:=StringToInt(Symbol.InfoNumber);
      if Measure>XmlStateData.MaxMeasure then XmlStateData.MaxMeasure:=Measure;
      if Counts>MaxCounts then
        MaxCounts:=Counts;
      Counts:=0;
      XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
      end
    else if Symbol.StateSymbol=XmlStateDurationEnd then
      begin
      // Save durations > 0 and find biggest common divisor
      if StringToInt(Symbol.Infobefore)>0 then
        begin
      if XmlState=XmlStateBackupBegin then
        Counts:=Counts-StringToInt(Symbol.Infobefore)
      else if XmlState=XmlStatePitchBegin then
        Counts:=Counts+StringToInt(Symbol.Infobefore)
      else if XmlState=XmlStateForwardBegin then
        Counts:=Counts+StringToInt(Symbol.Infobefore);
        if Counts>MaxCounts then MaxCounts:=Counts;
          end;
      Symbol.StateSymbol:=XmlStateDurationEnd;
      end
    else if Symbol.StateSymbol=XmlStateBackupBegin then
      begin
      XmlState:=XmlStateBackupBegin;
      end
    else if Symbol.StateSymbol=XmlStateChord then
      begin
      XmlState:=XmlStateChord;
      end
    else if Symbol.StateSymbol=XmlStateForwardBegin then
      begin
      if XmlState<>XmlStateChord then XmlState:=XmlStateForwardBegin;
      end
    else if Symbol.StateSymbol=XmlStatePitchBegin then
      begin
      if XmlState<>XmlStateChord then XmlState:=XmlStatePitchBegin;
      end
    else if Symbol.StateSymbol=XmlStateNoteBegin then
    with XmlStateData do
      begin
      XmlState:=XmlStateNoteBegin;
      end
    else if Symbol.StateSymbol=XmlStateLyricBegin then
    with XmlStateData do
      begin
      v:=StringToInt(Symbol.InfoNumber);
      if v>XmlStateData.MaxVerseNumberPart then
      XmlStateData.MaxVerseNumberPart:=v;

      if v>VerseNumberMapLast[PartNumber] then VerseNumberMapLast[PartNumber]:=v;
      if v<VerseNumberMapFirst[PartNumber] then VerseNumberMapFirst[PartNumber]:=v;
      end
    until (Symbol.StateSymbol=XmlStateEnd) or (DataInCount<=0);
  FileClose(DataIn);

  // Find smallest number divisible by all divisions
  if XmlStateData.DivisionListIndex=0 then
    Multiplicator:=1
  else
    begin
    Multiplicator:=DivisionList[0];
    for i:=1 to XmlStateData.DivisionListIndex-1 do
      begin
      m:=Multiplicator;
      Multiplicator:=Multiplicator*DivisionList[i];
      j:=0;
      while j<MaxPrime do
        begin
        if ((Multiplicator mod Prime[j])=0) and
           (((Multiplicator div Prime[j]) mod DivisionList[i])=0) and
            (((Multiplicator div Prime[j]) mod m)=0) then
          begin
          Multiplicator:=Multiplicator div Prime[j];
          end
        else
          begin
          inc(j);
          end;
        end;
      end;
    end;

  XmlStateData.CommonMultiplicator:=Multiplicator; ////
  end;


//---------------------------------------------------------------------------
//
//     Function:    PushRepeat
//
//     Purpose:     To handle nested repeats by pushing and popping the
//                  current repeat info. The current repeat is in common
//                  memory. The top of stack is the last.
//
//     Parameters:  Info = the saved data about the return position
//
//     Returns:     void
//
//     Notes:       Allow nested repeats? A nested repeat may not be
//                  legal in music - but never the less it occurs in
//                  many note-sheets.
//
//---------------------------------------------------------------------------

procedure PushRepeat(Info: TRepeatInfo);

  begin
  XmlStateData.RepeatInfo[XmlStateData.RepeatIndex]:=Info;
  inc(XmlStateData.RepeatIndex);
  end;

//---------------------------------------------------------------------------
//
//     Function:    PopRepeat
//
//     Purpose:     To handle nested repeats by pushing and popping the
//                  current repeat info. The current repeat is in common
//                  memory. The top of stack is the last.
//
//     Parameters:  FileName = the MusicXml file name
//
//     Returns:     The previous repeat information
//
//     Notes:       Allow nested repeats? A nested repeat may not be
//                  legal in music - but never the less it occurs in
//                  many note-sheets.
//
//---------------------------------------------------------------------------

function PopRepeat: TRepeatInfo;

  begin
  // The MusicXml file may have more end-repeats than begin-repeats
  if XmlStateData.RepeatIndex>0 then
    begin
    dec(XmlStateData.RepeatIndex);
    if XmlStateData.RepeatIndex<XmlStateData.RepeatIndexMin then
      XmlStateData.RepeatIndexMin:=XmlStateData.RepeatIndex;
    PopRepeat:=XmlStateData.RepeatInfo[XmlStateData.RepeatIndex];
    end
  else
    begin
    PopRepeat:=XmlStateData.RepeatInfoCurrent;
    XmlStateData.RepeatIndex:=0;
  end;
  end;

//---------------------------------------------------------------------------
//
//     Function:    LookupPartId
//
//     Purpose:     The parts number ////
//
//     Parameters:  s = A part id to be searched for
//
//     Returns:     The part number
//
//     Notes:       ////
//
//---------------------------------------------------------------------------

function LookupPartId(s: string): integer;

var i: integer;
    r: integer;

  begin
  r:=-1;  // Means not found
  for i:=0 to XmlStateData.PartNumberMax-1 do
    begin
    if XmlStateData.PartMap[i]=s then r:=i;
    end;
  LookupPartId:=r;
  end;

//---------------------------------------------------------------------------
//
//     Function:    LookUpMidiInstrument
//
//     Purpose:     ////
//
//     Parameters:  s = ////
//
//     Returns:     The instrument number ???? ////
//
//     Notes:       ////
//
//---------------------------------------------------------------------------

function LookUpMidiInstrument(s: string; part: integer): integer;

var i: integer;
    r: integer;

  begin
  r:=-1;  // Means not found
  for i:=0 to XmlStateData.MidiInstrumentMax-1 do
    begin
    if (XmlStateData.MidiInstrument[i]=s) and
       (XmlStatedata.MidiInstrumentPart[i]=part) then
      r:=i;
    end;
  LookupMidiInstrument:=r;
  end;

//---------------------------------------------------------------------------
//
//     Function:    ReadXmlToMidiFile
//
//     Purpose:     Process the MusicXml file and generate midi data
//
//     Parameters:  FileName = the MusicXml file name
//                  MidiData = the generated data for Midi
//
//     Returns:     void
//
//     Notes:       A state machine processes the Xml syntax
//
//---------------------------------------------------------------------------

procedure ReadXmlToMidiFile(FileName: string; var MidiData: TMidiData);

var Symbol: TSymbol;        ////
    i: integer;         ////
    j: integer;
    s: string;
    p: integer; // Position of some text
    newvolume: integer; // When computing new volume

//// In start of every measure check Segno, Coda and Dacapo
procedure SetSegnoCodaCapo;

  begin
  if DirectionSegno in MeasureDirections[XmlStateData.MeasureNumber] then
    begin
    XmlStateData.RepeatInfoSegno.RepeatStart:=GetFilePos(DataIn);
    XmlStateData.RepeatInfoSegno.RepeatStartLineNumber:=XmlLineNumber;
    XmlStateData.RepeatInfoSegno.RepeatMeasureNumber:=XmlStateData.MeasureNumber;
    XmlStateData.RepeatInfoSegno.RepeatStartState:=XmlStateMeasureBegin;
    XmlStateData.RepeatInfoSegno.RepeatBeginOpen:=true;
    XmlStateData.RepeatInfoSegno.RepeatNoteNumber:=XmlStateData.NoteNumber;
    // If used as music subroutine, use this to reset tempo
    if XmlStateData.MusicSubroutine.RepeatTempo=0 then
    XmlStateData.MusicSubroutine.RepeatTempo:=XmlStateData.Tempo;
    end;

  if DirectionCoda in MeasureDirections[XmlStateData.MeasureNumber] then
    begin
    XmlStateData.RepeatInfoCoda.RepeatStart:=GetFilePos(DataIn);
    XmlStateData.RepeatInfoCoda.RepeatStartLineNumber:=XmlLineNumber;
    XmlStateData.RepeatInfoCoda.RepeatMeasureNumber:=XmlStateData.MeasureNumber;
    XmlStateData.RepeatInfoCoda.RepeatStartState:=XmlStateMeasureBegin;
    XmlStateData.RepeatInfoCoda.RepeatBeginOpen:=true;
    XmlStateData.RepeatInfoCoda.RepeatNoteNumber:=XmlStateData.NoteNumber;
    end;
  end;

procedure CheckSegnoCodaCapo;

var m: Integer;   // Measure

  begin
  if (DirectionDalsegno in MeasureDirections[XmlStateData.MeasureNumber]) and
     (XmlStateData.RepeatInfoSegno.RepeatNumber=0) then
    begin
    // Music subroutines in makam music (refrain)
    XmlStateData.MusicSubroutine.RepeatStart:=GetFilePos(DataIn);
    XmlStateData.MusicSubroutine.RepeatStartLineNumber:=XmlLineNumber;
    XmlStateData.MusicSubroutine.RepeatMeasureNumber:=XmlStateData.MeasureNumber;
    XmlStateData.MusicSubroutine.RepeatStartState:=XmlState;
    XmlStateData.MusicSubroutine.RepeatBeginOpen:=true;
    XmlStateData.MusicSubroutine.RepeatNoteNumber:=XmlStateData.NoteNumber;

    if XmlStateData.MusicSubroutine.RepeatTempo>0 then
       begin
       XmlStateData.Tempo:=XmlStateData.MusicSubroutine.RepeatTempo;
       XmlStateData.TempoOrg:=XmlStateData.Tempo;
       end;

    SetFilePosition(DataIn,XmlStateData.RepeatInfoSegno.RepeatStart,0);
    ReadXmlBufferIndex:=0;
    XmlDataInCount:=0;
    XmlLineNumber:=XmlStateData.RepeatInfoSegno.RepeatStartLineNumber;
    XmlStateData.MeasureNumber:=XmlStateData.RepeatInfoSegno.RepeatMeasureNumber;
    XmlState:=XmlStateData.RepeatInfoSegno.RepeatStartState;
    XmlStateData.NoteNumber:=XmlStateData.RepeatInfoSegno.RepeatNoteNumber;
    inc(XmlStateData.RepeatInfoSegno.RepeatNumber);

    SetSegnoCodaCapo;
    end;

  if (DirectionDalSegnoAlCoda in MeasureDirections[XmlStateData.MeasureNumber]) and
     (XmlStateData.RepeatInfoSegno.RepeatNumber=0) then
    begin
    if not (XmlStateData.RepeatInfoSegno.RepeatStart>0) then
      XmlError('No Segno to go to')
    else
      begin
    SetFilePosition(DataIn,XmlStateData.RepeatInfoSegno.RepeatStart,0);
    ReadXmlBufferIndex:=0;
    XmlDataInCount:=0;
    XmlLineNumber:=XmlStateData.RepeatInfoSegno.RepeatStartLineNumber;
    XmlStateData.MeasureNumber:=XmlStateData.RepeatInfoSegno.RepeatMeasureNumber;
    XmlState:=XmlStateData.RepeatInfoSegno.RepeatStartState;
    XmlStateData.NoteNumber:=XmlStateData.RepeatInfoSegno.RepeatNoteNumber;
    inc(XmlStateData.RepeatInfoSegno.RepeatNumber);
    inc(XmlStateData.RepeatInfoCoda.RepeatNumber);
      SetSegnoCodaCapo
    end;
    end;

  // Jump to Coda due to Dacapo, Segno etc.
  if (DirectionToCoda in MeasureDirections[XmlStateData.MeasureNumber]) and
     (XmlStateData.RepeatInfoCoda.RepeatNumber=1) then
    begin
    if XmlStateData.RepeatInfoCoda.RepeatStart<>0 then
      begin // Coda point has been passed and filled in. Use it
    SetFilePosition(DataIn,XmlStateData.RepeatInfoCoda.RepeatStart,0);
    ReadXmlBufferIndex:=0;
    XmlDataInCount:=0;
    XmlLineNumber:=XmlStateData.RepeatInfoCoda.RepeatStartLineNumber;
    XmlStateData.MeasureNumber:=XmlStateData.RepeatInfoCoda.RepeatMeasureNumber;
    XmlState:=XmlStateData.RepeatInfoCoda.RepeatStartState;
      XmlStateData.NoteNumber:=CodaNoteNumber;
    inc(XmlStateData.RepeatInfoCoda.RepeatNumber);
      SetSegnoCodaCapo;
      end
    else
      begin // Coda point has not been passed, search for it
      // Read until right measure
        repeat
        Symbol:=NextSymbol;
        if Symbol.StateSymbol=XmlStateMeasureBegin then
          begin
          m:=StringToInt(Symbol.InfoNumber);
          XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
          end;
        until (Symbol.StateSymbol=XmlStateEnd) or
              ((Symbol.StateSymbol=XmlStateMeasureEnd) and
              (m=XmlStateData.RepeatInfoForwardCoda.RepeatMeasureNumber-1));
      XmlStateData.NoteNumber:=CodaNoteNumber;
      // Now we are no longer on first repeat
      XmlStateData.MeasureNumber:=m+1;             //// åååååååå



      XmlState:=XmlStateMeasureBegin;
      SetSegnoCodaCapo;

      inc(XmlStateData.RepeatInfoCoda.RepeatNumber);


      XmlStateData.RepeatInfoCapo.RepeatNumber:=0; //// ååååååååååå



      end

    end;

  if DirectionDacapo in MeasureDirections[XmlStateData.MeasureNumber] then
    begin
    XmlStateData.RepeatInfoFine.RepeatStart:=GetFilePos(DataIn);
    XmlStateData.RepeatInfoFine.RepeatMeasureNumber:=XmlStateData.MeasureNumber;
    XmlStateData.RepeatInfoFine.RepeatStartLineNumber:=XmlLineNumber;
    XmlStateData.RepeatInfoFine.RepeatStartState:=XmlState;
    XmlStateData.RepeatInfoFine.RepeatBeginOpen:=true;
    XmlStateData.RepeatInfoFine.RepeatNoteNumber:=XmlStateData.NoteNumber;
    end;

  if DirectionDacapoAlFine in MeasureDirections[XmlStateData.MeasureNumber] then
    begin
    XmlStateData.RepeatInfoFine.RepeatStart:=GetFilePos(DataIn);
    XmlStateData.RepeatInfoFine.RepeatMeasureNumber:=XmlStateData.MeasureNumber;
    XmlStateData.RepeatInfoFine.RepeatStartLineNumber:=XmlLineNumber;
    XmlStateData.RepeatInfoFine.RepeatStartState:=XmlState;
    XmlStateData.RepeatInfoFine.RepeatBeginOpen:=true;
    XmlStateData.RepeatInfoFine.RepeatNoteNumber:=XmlStateData.NoteNumber;
    end;

  if DirectionDalsegnoAlFine in MeasureDirections[XmlStateData.MeasureNumber] then
    begin
    XmlStateData.RepeatInfoFine.RepeatStart:=GetFilePos(DataIn);
    XmlStateData.RepeatInfoFine.RepeatMeasureNumber:=XmlStateData.MeasureNumber;
    XmlStateData.RepeatInfoFine.RepeatStartLineNumber:=XmlLineNumber;
    XmlStateData.RepeatInfoFine.RepeatStartState:=XmlState;
    XmlStateData.RepeatInfoFine.RepeatBeginOpen:=true;
    XmlStateData.RepeatInfoFine.RepeatNoteNumber:=XmlStateData.NoteNumber;
    end;

  if (DirectionDacapo in MeasureDirections[XmlStateData.MeasureNumber]) and
     (XmlStateData.RepeatInfoCapo.RepeatNumber=0) then
    begin
    SetFilePosition(DataIn,XmlStateData.VerseStart,0);
    ReadXmlBufferIndex:=0;
    XmlDataInCount:=0;
    XmlLineNumber:=XmlStateData.RepeatInfoCapo.RepeatStartLineNumber;
    XmlStateData.MeasureNumber:=1;
    XmlState:=XmlStateRepeatPartBegin;
    XmlStateData.NoteNumber:=0;
    inc(XmlStateData.RepeatInfoCapo.RepeatNumber);
    inc(XmlStateData.RepeatInfoCoda.RepeatNumber);
    SetSegnoCodaCapo;
    end;

  if (DirectionDacapoAlCoda in MeasureDirections[XmlStateData.MeasureNumber]) and
     (XmlStateData.RepeatInfoCapo.RepeatNumber=0) then
    begin
    SetFilePosition(DataIn,XmlStateData.VerseStart,0);
    ReadXmlBufferIndex:=0;
    XmlDataInCount:=0;
    XmlLineNumber:=XmlStateData.VerseStartLineNumber;
    XmlStateData.MeasureNumber:=XmlStateData.VerseStartMeasureNumber;

    XmlStateData.MeasureNumber:=1;
    XmlState:=XmlStateRepeatPartBegin;
    XmlStateData.NoteNumber:=0;
    inc(XmlStateData.RepeatInfoCapo.RepeatNumber);
    inc(XmlStateData.RepeatInfoCoda.RepeatNumber);
    SetSegnoCodaCapo;
    end;

  if (DirectionDacapoAlFine in MeasureDirections[XmlStateData.MeasureNumber]) and
     (XmlStateData.RepeatInfoFine.RepeatNumber=0) then
    begin
    SetFilePosition(DataIn,XmlStateData.VerseStart,0);
    ReadXmlBufferIndex:=0;
    XmlDataInCount:=0;
    XmlLineNumber:=XmlStateData.RepeatInfoCapo.RepeatStartLineNumber;
    XmlStateData.MeasureNumber:=1;
    XmlStateData.NoteNumber:=XmlStateData.RepeatInfoCapo.RepeatNoteNumber;
    XmlState:=XmlStateRepeatPartBegin;
    inc(XmlStateData.RepeatInfoCapo.RepeatNumber);
    inc(XmlStateData.RepeatInfoCoda.RepeatNumber);
    inc(XmlStateData.RepeatInfoFine.RepeatNumber);
    SetSegnoCodaCapo;
    end;



  if (DirectionDalsegnoAlFine in MeasureDirections[XmlStateData.MeasureNumber]) and
     (XmlStateData.RepeatInfoFine.RepeatNumber=0) then
    begin
    SetFilePosition(DataIn,XmlStateData.RepeatInfoSegno.RepeatStart,0);
    ReadXmlBufferIndex:=0;
    XmlDataInCount:=0;
    XmlLineNumber:=XmlStateData.RepeatInfoSegno.RepeatStartLineNumber;
    XmlStateData.MeasureNumber:=XmlStateData.RepeatInfoSegno.RepeatMeasureNumber;
    XmlState:=XmlStateData.RepeatInfoSegno.RepeatStartState;
    XmlStateData.NoteNumber:=XmlStateData.RepeatInfoSegno.RepeatNoteNumber;
    inc(XmlStateData.RepeatInfoFine.RepeatNumber);
    SetSegnoCodaCapo;
    end;

  if (DirectionFine in MeasureDirections[XmlStateData.MeasureNumber]) and
      (XmlStateData.RepeatInfoFine.RepeatNumber=1) then
    begin
    SetFilePosition(DataIn,XmlStateData.RepeatInfoFine.RepeatStart,0);
    ReadXmlBufferIndex:=0;
    XmlDataInCount:=0;
    XmlLineNumber:=XmlStateData.RepeatInfoFine.RepeatStartLineNumber;
    XmlStateData.MeasureNumber:=XmlStateData.RepeatInfoFine.RepeatMeasureNumber;
    XmlStateData.NoteNumber:=XmlStateData.RepeatInfoFine.RepeatNoteNumber;
    // Eventually implicit repeat in the end shall be ignored with Fine
    XmlStateData.RepeatIndex:=XmlStateData.RepeatIndexMin;
    XmlState:=XmlStateData.RepeatInfoFine.RepeatStartState;
    inc(XmlStateData.RepeatInfoCapo.RepeatNumber);
    inc(XmlStateData.RepeatInfoCoda.RepeatNumber);
    end;
  end;

  // Body of ReadXmlToMidiFile;

  begin
  XmlStateData.MaxVerseCheck:=true;
  XmlStateData.MeasureNumber:=1;
  XmlStateData.MicroTones:=false;  
  // I.e. until all part have same number of verses
  XmlInit;

  XmlStateData.Utf16A:=false;
  XmlStateData.Utf16B:=false;
  XmlStateData.PartNumber:=0;
  XmlStateData.TrackNumber:=0;
  XmlStateData.NoteIsGraceNote:=false;

  PreReadXmlDivisions(FileName);

  XmlStateData.Utf16A:=false;
  XmlStateData.Utf16B:=false;
  XmlStateData.PartNumber:=0;
  XmlStateData.TrackNumber:=0;
  XmlStateData.NoteIsGraceNote:=false;

  PreReadXmlToMidiFile(FileName);

  for VerseCheckCount:=1 to 2 do if XmlStateData.MaxVerseCheck then
    begin
  XmlStateData.Utf16A:=false;
  XmlStateData.Utf16B:=false;
  XmlStateData.PartNumber:=0;
  XmlStateData.TrackNumber:=0;
  XmlStateData.NoteIsGraceNote:=false;

////  XmlInit;
{$ifdef FPC}
{$ifdef Darwin}
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(FileName,fmOpenRead or fmShareDenyNone);
{$else}
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(FileName,fmOpenRead or fmShareDenyNone)
  else
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(FileName,fmOpenRead or fmShareDenyNone);
{$endif}
{$else}
  if (FileName<>'') and (FileExists(FileName)) then
    DataIn:=FileOpen(PChar(FileName),fmOpenRead or fmShareDenyNone);
////    messagebox(0,PChar(FileName),PChar('Opening file'+IntToStr(ord(DataIn))),mb_ok);
{$endif}
  //// Skulle det til XmlInit?

  XmlState:=XmlStateIdentifyUtf;
  DataInCount:=1;  // Set not end of file
  XmlStateData.CreditType:='';
  XmlStateData.NoteDelta:=0;
  XmlStateData.Chord:=false;
  XmlStateData.ParsingError:=false;

  XmlStateData.TrackIndex:=0;
  XmlStateData.SongTitle:='';
  XmlStateData.SongTitleCodePage:='';
  XmlStateData.MeasureNumber:=1;
  XmlStateData.PartNumber:=0;
  XmlStateData.TrackNumber:=0;
  XmlStateData.Duration:=0;
  XmlStateData.PartId:='';
  XmlStateData.Ritardando:=0;

// mmmmmmmmmmmmmmmmmmmmmmmmmmmmmmmm   //// OBS OPTIMERE ????
////  XmlStateData.XmlTimeIndexMax:=NumberOfTimeSamples-1;
  XmlStateData.XmlTimeIndexMax:=XmlStateData.MaxIndex;

  XmlStateData.Divisions:=1;
  XmlStateData.Fifths:='';  ////
  XmlStateData.Mode:='';
  XmlStateData.ActualNotes:='4';
  XmlStateData.NormalNotes:='4';

  for i:=0 to Tracks do
    begin
    XmlStateData.PartNames[i]:='';
    XmlStateData.MidiProgram[i]:=0;
    XmlStateData.PartNames[i]:='Channel'+IntToStr(i);
    for j:=0 to 127 do
      begin
      XmlStateData.Tie[i,j]:=false;
      XmlStateData.NoteOn[i,j]:=false;
      end;
    end;

  XmlStateData.TieStart:=false;
  XmlStateData.TieStop:=false;
  XmlStateData.PartNumber:=0;
  XmlStateData.TrackNumber:=0;
  XmlStateData.MeasureNumber:=0;
  XmlStateData.VoltaText:='';
    XmlStateData.VoltaTextRepeat:='';
  XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;
  XmlStateData.VerseLast:=0;
  XmlStateData.MidiInstrumentMax:=0;
  XmlStateData.ScoreInstrumentMax:=0;
  XmlStateData.XmlTimeIndexStartMerge:=0;
  XmlStateData.XmlTimeIndexMiddleMerge:=0;
  XmlStateData.XmlTimeIndexStopMerge:=0;
    XmlStateData.MoreLyric:=false;
////    XmlStateData.MicroTones:=false;
  // Not EOF
  DataInCount:=1;
  ReadXmlBufferIndex:=0;
  XmlDataInCount:=0;
  // The line number is used for error messages only. Refers to MusicXml file.
  XmlLineNumber:=1;

  //// REPEAT !!!!
  while (Symbol.StateSymbol<>XmlStateEnd) and (XmlState<>XmlStateEnd) and
       (XmlState<>XmlStateError) do
    begin
      Symbol:=NextSymbol;

      //// TEST !!!!
        if XmlLineNumber=433 then
          begin
          XmlState:=XmlState;
          end;

        if XmlLineNumber=240 then
        begin
          XmlState:=XmlState;
          end;

      case XmlState of
      XmlStateBegin:
        begin
        if (Symbol.StateSymbol=XmlStateVersion) {and
           (Symbol.InfoValue='"1.0" encoding="UTF-8"?'}   //// KOMMER VI HER?
          then
          XmlState:=XmlStateVersion
        else
          XmlError(LinguaTextNoXmlHeaderFoundBut+Symbol.InfoName+'"');
        end;
      XmlStateVersion:
        begin
        if  (Symbol.StateSymbol=XmlStateDocType) and
            (Pos('MusicXML',Symbol.InfoDocType)>0) then
            //// TEST VERSION M.M.?
          XmlState:=XmlStateDocType
        end;
      XmlStateDocType:
        begin
        if Symbol.StateSymbol=XmlStateScorePartwiseBegin then
          XmlState:=XmlStateScorePartwiseBegin
        end;

      XmlStateWorkEnd,
      XmlStateTitleEnd,
      XmlStateMovementTitleEnd,
      XmlStateScorePartwiseBegin:
        begin
        if Symbol.StateSymbol=XmlStateCreditBegin then
          XmlState:=XmlStateCreditBegin
        else if Symbol.StateSymbol=XmlStatePartListBegin then
          XmlState:=XmlStatePartListBegin
        else if Symbol.StateSymbol=XmlStateWorkBegin then
          XmlState:=XmlStateWorkBegin
        else if Symbol.StateSymbol=XmlStateMovementTitleBegin then
          XmlState:=XmlStateMovementTitleBegin
        else if Symbol.StateSymbol=XmlStateTitleBegin then
          XmlState:=XmlStateTitleBegin
        end;
      XmlStateScorePartwiseEnd:
        begin
        //// XmlState:=XmlStateEnd; ????
        end;

      XmlStateWorkBegin:
        begin
        if Symbol.StateSymbol=XmlStateWorkTitleBegin then
        XmlState:=XmlStateWorkTitleBegin;
        end;
      XmlStateWorkTitleBegin:
        begin
        if Symbol.StateSymbol=XmlStateWorkTitleEnd then
          begin
          XmlStateData.SongTitle:=Symbol.InfoBefore;
          XmlState:=XmlStateWorkTitleEnd;
          end;
        end;

      XmlStateMovementTitleBegin:
        begin
        if Symbol.StateSymbol=XmlStateMovementTitleEnd then
          begin
          if XmlStateData.SongTitle='' then
          XmlStateData.SongTitle:=Symbol.InfoBefore;
          XmlState:=XmlStateMovementTitleEnd;
          end;
        end;

      XmlStateTitleBegin:
        begin
        if Symbol.StateSymbol=XmlStateMovementTitleEnd then
          begin
          if XmlStateData.SongTitle='' then
          XmlStateData.SongTitle:=Symbol.InfoBefore;
          XmlState:=XmlStateTitleEnd;
          end;
        end;

      XmlStateWorkTitleEnd:
        begin
        if Symbol.StateSymbol=XmlStateWorkEnd then
          XmlState:=XmlStateWorkEnd;
        end;

      XmlStatePartListBegin:
        begin
        if Symbol.StateSymbol=XmlStateScorePartBegin then
          begin
          XmlStateData.PartId:=Symbol.InfoId;                ////
          XmlState:=XmlStateScorePartBegin;
          XmlStateData.BeforeFirstMeasure:=true;
          XmlStateData.BeforeFirstPart:=true;
          XmlStateData.FirstPartNumber:=0;
          XmlStateData.TrackNumber:=0;
          XmlStateData.MeasureTime:=0;
          XmlStateData.VoltaText:='';
            XmlStateData.VoltaTextRepeat:='';
          NoteInfoClear;
          end;
        end;

      XmlStateScorePartBegin:
        begin
          // Parts numbered 0..PartNumber
        XmlStateData.PartNumber:=LookUpPartId(XmlStateData.PartId);
        if XmlStateData.PartNumber<0 then XmlStateData.PartNumber:=0;
        // The verse number is reset by this variable and offset
          XmlStateData.BeforeFirstMeasure:=true;
          XmlStateData.MeasureTime:=0;
        if Symbol.StateSymbol=XmlStatePartNameBegin then
          XmlState:=XmlStatePartNameBegin
        else if Symbol.StateSymbol=XmlStatePartName then
          begin
          //// Indlæs eventuelt navn - se  (state = XmlStatePartNameBegin) Fanny Power
          end
        else if Symbol.StateSymbol=XmlStateScorePartEnd then
          XmlState:=XmlStateScorePartEnd
        end;

      XmlStateMidiInstrumentBegin,
      XmlStateMidiProgramEnd,
      XmlStatePanEnd,
      XmlStateVolumeEnd,
      XmlStateMidiUnpitchedEnd,
      XmlStateMidiChannelEnd:
        begin
        if Symbol.StateSymbol=XmlStateMidiProgramBegin then
          XmlState:=XmlStateMidiProgramBegin
        else if Symbol.StateSymbol=XmlStatePanBegin then
          XmlState:=XmlStatePanBegin
        else if Symbol.StateSymbol=XmlStateVolumeBegin then
          XmlState:=XmlStateVolumeBegin
        else if Symbol.StateSymbol=XmlStateMidiChannelBegin then
          begin
          XmlState:=XmlStateMidiChannelBegin;
          end
        else if Symbol.StateSymbol=XmlStateMidiInstrumentEnd then
          begin
          with XmlStateData do
            begin
            i:=LookUpMidiInstrument(MidiInstrumentId,PartNumber);

////            if i>64 then    <---- Hej Hej Marie
////            i:=LookUpMidiInstrument(MidiInstrumentId,PartNumber);


            if (i<0) or (i>MaxNumberOfInstruments) then
              begin // Not found, add it and set the information given.
            MidiInstrument[MidiInstrumentMax]:=XmlStateData.MidiInstrumentId;
            MidiInstrumentChannel[MidiInstrumentMax]:=XmlStateData.MidiChannelNumber;
            MidiInstrumentProgram[MidiInstrumentMax]:=XmlStateData.MidiProgramNumber;
              MidiInstrumentPart[MidiInstrumentMax]:=PartNumber;
              MidiInstrumentPitch[MidiInstrumentMax]:=XmlStateData.Unpitched[XmlStateData.PartNumber]-1;
              XmlStateData.Pan[MidiInstrumentMax]:=XmlStateData.CurrentMidiPanorama;
              XmlStateData.Volume[MidiInstrumentMax]:=XmlStateData.CurrentMidiVolume;
              if MidiInstrumentMax>=127 then
              XmlError('The score specifies endless loop (Check Segno and Coda)')
              else
            inc(MidiInstrumentMax);
              end
            else
              begin // Found, change default information to information given.
              MidiInstrument[i]:=XmlStateData.MidiInstrumentId;
              MidiInstrumentChannel[i]:=XmlStateData.MidiChannelNumber;
              MidiInstrumentProgram[i]:=XmlStateData.MidiProgramNumber;
              //// VOLUME PAN !!!!
              MidiInstrumentPart[i]:=PartNumber;
              MidiInstrumentPitch[i]:=XmlStateData.Unpitched[XmlStateData.PartNumber]-1;

              if i>127 then  //// TEST !!!! ////ci
                XmlError('Bad score structure - possibly exist To Segno but no Segno')
              else
                begin
              XmlStateData.Pan[i]:=XmlStateData.CurrentMidiPanorama;
              XmlStateData.Volume[i]:=XmlStateData.CurrentMidiVolume;
                end;
              end
            end;
          XmlState:=XmlStateMidiInstrumentEnd
          end
        else if Symbol.StateSymbol=XmlStateMidiUnpitchedBegin then
          XmlState:=XmlStateMidiUnpitchedBegin
//// Accept this 
////        else
////          XmlError('Unknown definition ('+Symbol.InfoName+') in "midi-instrument"');
        end;

      XmlStateMidiChannelBegin:
        begin
        if Symbol.StateSymbol=XmlStateMidiChannelEnd then
          begin
////          Nogle xml filer har bare "<midi-channel>1</midi-channel>"
          i:=StringToInt(Symbol.InfoBefore)-1;
          if i<0 then i:=15 else if i>15 then i:=15;
          XmlStateData.MidiChannel[XmlStateData.PartNumber]:=i;

          XmlStateData.MidiChannelNumber:=i;
          XmlState:=XmlStateMidiChannelEnd
          end
        else
          XmlError('No "/midi-channel"');
        end;

      XmlStateMidiProgramBegin:
        begin
        if Symbol.StateSymbol=XmlStateMidiProgramEnd then
          begin
////          if XmlStateData.PartNumber=0 then
////            XmlError('Error in Xml-file: PartNumber = '+IntToStr(XmlStateData.PartNumber));

          if (StringToInt(Symbol.InfoBefore)=0) or (StringToInt(Symbol.InfoBefore)>128) then
            XmlStateData.MidiProgram[XmlStateData.PartNumber]:=0 // Use piano
          else

            //// Undrer om ikke hellere det skulle være under <midi-instrument>
            //// att værdien gemmes.
{$ifdef UseMidiChannels}
            XmlStateData.MidiProgram[XmlStateData.MidiChannel[XmlStateData.PartNumber]]:=
                                 StringToInt(Symbol.InfoBefore)-1; // Use specified
{$else}
            XmlStateData.MidiProgram[XmlStateData.PartNumber]:=
                                 StringToInt(Symbol.InfoBefore)-1; // Use specified
{$endif}
          if StringToInt(Symbol.InfoBefore) in [1..128] then
          XmlStateData.MidiProgramNumber:=StringToInt(Symbol.InfoBefore)-1
          else
          XmlStateData.MidiProgramNumber:=0;
          XmlState:=XmlStateMidiProgramEnd
          end
        else
          XmlError('No "/midi-program"');
        end;

      XmlStateVolumeBegin:
        begin
        if Symbol.StateSymbol=XmlStateVolumeEnd then
          begin
          i:=StringToInt(Symbol.InfoBefore);
          if i>127 then
            XmlStateData.CurrentMidiVolume:=127
          else if i<0 then
            XmlStateData.CurrentMidiVolume:=0
          else
            XmlStateData.CurrentMidiVolume:=i;
          XmlStateData.Volume[XmlStateData.TrackNumber]:=XmlStateData.CurrentMidiVolume;
          XmlState:=XmlStateVolumeEnd
          end
        else
          XmlError('No "/midi-program"');
        end;



      XmlStateMidiUnpitchedBegin:
        begin
        if Symbol.StateSymbol=XmlStateMidiUnpitchedEnd then
          begin
          XmlStateData.Unpitched[XmlStateData.PartNumber]:=
                                        StringToInt(Symbol.InfoBefore);
          XmlState:=XmlStateMidiUnpitchedEnd
          end
        else
          XmlError('No "/midi-program"');
        end;



      XmlStatePanBegin:
        begin
        if Symbol.StateSymbol=XmlStatePanEnd then
          begin // Panorama -90 to 90 converted to 0..127
          i:=(((90+StringToInt(Symbol.InfoBefore))*127) div 180);
          if i<0 then XmlStateData.CurrentMidiPanorama:=0
          else if i>127 then XmlStateData.CurrentMidiPanorama:=127
          else XmlStateData.CurrentMidiPanorama:=i;
          XmlState:=XmlStatePanEnd;
          end
        else
          XmlError('No "/midi-program"');
        end;

      XmlStatePartNameBegin:
        begin
        if Symbol.StateSymbol=XmlStatePartNameEnd then
          begin
////          XmlStateData.Partnames[XmlStateData.PartNumber]:=UtfToCodepage(Symbol.InfoBefore);
          XmlStateData.Partnames[XmlStateData.PartNumber]:=Symbol.InfoBefore;
          XmlState:=XmlStatePartNameEnd;
          end;
        end;

      XmlStateMidiInstrumentEnd:
        begin
        if Symbol.StateSymbol=XmlStateScorePartEnd then
          begin
          XmlState:=XmlStateScorePartEnd
          end
        else if Symbol.StateSymbol=XmlStateMidiInstrumentBegin then
          begin
          with XmlStateData do
            begin
            // Default values are set in case definition is missing
            XmlStateData.MidiInstrumentId:=Symbol.InfoId;
            XmlStateData.CurrentMidiVolume:=MidiDefaultVolume;
            XmlStateData.CurrentMidiPanorama:=MidiDefaultPanorama;
            end;
          XmlState:=XmlStateMidiInstrumentBegin;
          end
        end;

      XmlStateScoreInstrumentEnd:
        begin
        if Symbol.StateSymbol=XmlStateScoreInstrumentBegin then
          begin
          with XmlStateData do
            begin
            XmlStateData.ScoreInstrumentId:=Symbol.InfoId;
            end;
          XmlState:=XmlStateScoreInstrumentBegin
          end
        else if Symbol.StateSymbol=XmlStateScorePartEnd then
          begin
          XmlState:=XmlStateScorePartEnd
          end
        else if Symbol.StateSymbol=XmlStateMidiInstrumentBegin then
          begin
          with XmlStateData do
            begin
            XmlStateData.MidiInstrumentId:=Symbol.InfoId;
            XmlStateData.CurrentMidiVolume:=MidiDefaultVolume;
            XmlStateData.CurrentMidiPanorama:=MidiDefaultPanorama;
            end;
          XmlState:=XmlStateMidiInstrumentBegin;
          end
        end;

      XmlStatePartNameEnd:
        begin
        if Symbol.StateSymbol=XmlStateScorePartEnd then
          XmlState:=XmlStateScorePartEnd
        else if Symbol.StateSymbol=XmlStateMidiInstrumentBegin then
          begin
          with XmlStateData do
            begin
            XmlStateData.MidiInstrumentId:=Symbol.InfoId;
            XmlStateData.CurrentMidiVolume:=MidiDefaultVolume;
            XmlStateData.CurrentMidiPanorama:=MidiDefaultPanorama;
            end;
          XmlState:=XmlStateMidiInstrumentBegin;
          end
        else if Symbol.StateSymbol=XmlStateScoreInstrumentBegin then
          begin
          with XmlStateData do
            begin
            XmlStateData.ScoreInstrumentId:=Symbol.InfoId;
            end;
          XmlState:=XmlStateScoreInstrumentBegin;
          end;
////        else
////          XmlError('Unknown definition ('+Symbol.InfoName+') in "midi-instrument"');

        end;


      XmlStateScoreInstrumentBegin,
      XmlStateInstrumentNameEnd:
        begin
        if Symbol.StateSymbol=XmlStateInstrumentNameBegin then
          begin
          XmlStateData.InstrumentName:=Symbol.InfoBefore;
          XmlState:=XmlStateInstrumentNameBegin
          end
        else if Symbol.StateSymbol=XmlStateScoreInstrumentEnd then
          begin
          with XmlStateData do
            begin
            // The Score-part should have a corresponding Midi-definition, but...
            ScoreInstrument[ScoreInstrumentMax]:=XmlStateData.ScoreInstrumentId;
            ScoreInstrumentNames[ScoreInstrumentMax]:=XmlStateData.InstrumentName;
            ScoreInstrumentPart[ScoreInstrumentMax]:=XmlStateData.PartNumber;
            if ScoreInstrumentMax>=127 then
              XmlError('The score specifies endless loop (Check Segno and Coda)')
            else
            inc(ScoreInstrumentMax);

            // Default data for MidiInstrument
            MidiInstrument[MidiInstrumentMax]:=XmlStateData.ScoreInstrumentId;
            MidiInstrumentChannel[MidiInstrumentMax]:=0;
            MidiInstrumentProgram[MidiInstrumentMax]:=0;
            MidiInstrumentPart[MidiInstrumentMax]:=PartNumber;
            MidiInstrumentPitch[MidiInstrumentMax]:=XmlStateData.Unpitched[XmlStateData.PartNumber]-1;
            if MidiInstrumentMax>=127 then
              XmlError('The score specifies endless loop (Check Segno and Coda)')
            else
            inc(MidiInstrumentMax);

            end;
          if XmlState<>XmlStateError then XmlState:=XmlStateScoreInstrumentEnd
          end
//        else
//          XmlError('Unknown definition ('+Symbol.InfoName+') in "midi-instrument"');
        end;

      XmlStateInstrumentNameBegin:
        begin
        if Symbol.StateSymbol=XmlStateInstrumentNameEnd then
          begin
          XmlStateData.InstrumentName:=Symbol.InfoBefore;
          XmlState:=XmlStateInstrumentNameEnd;
          end
        else
          XmlError('No "/midi-program"');
        end;

      XmlStatePartListEnd:
        begin
        if Symbol.StateSymbol=XmlStatePartListBegin then
          XmlState:=XmlStatePartListBegin
        else if Symbol.StateSymbol=XmlStateCreditBegin then
          XmlState:=XmlStateCreditBegin
        else if Symbol.StateSymbol=XmlStatePartListBegin then
          begin
          XmlStateData.PartId:=Symbol.InfoId;
          XmlState:=XmlStatePartListBegin
          end
        else if Symbol.StateSymbol=XmlStatePartBegin then
          begin
          XmlStateData.PartId:=Symbol.InfoId;

          XmlStateData.PartNumber:=LookUpPartId(XmlStateData.PartId);
          if XmlStateData.PartNumber>=0 then
          XmlStateData.CurrentVolume:=XmlStateData.Volume[XmlStateData.TrackNumber];
          NoteInfoGraceIndex:=1;
          NoteInfoGraceIndexStart:=1;
          XmlStateData.CurrentVolumeStart:=0;
          XmlStateData.VoltaText:='';
            XmlStateData.VoltaTextRepeat:='';
          XmlStateData.VerseNumber:=1;
          XmlStateData.VerseStart:=GetFilePos(DataIn);
          XmlStateData.VerseStartLineNumber:=XmlLineNumber;
          XmlStateData.VerseStartMeasureNumber:=XmlStateData.MeasureNumber;
          XmlStateData.StaffNumber:=1;
          XmlStateData.RepeatInfoCurrent.RepeatStart:=GetFilePos(DataIn);
          XmlStateData.RepeatInfoCurrent.RepeatBeginOpen:=false;
          XmlStateData.RepeatInfoCurrent.RepeatStartLineNumber:=XmlLineNumber;
          XmlStateData.RepeatInfoCurrent.RepeatStartState:=XmlStateRepeatPartBegin;
          XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;
            XmlStateData.RepeatInfoCurrent.RepeatNoteNumber:=XmlStateData.NoteNumber;

          XmlStateData.RepeatIndex:=1;
          PushRepeat(XmlStateData.RepeatInfoCurrent);
          XmlStateData.RepeatIndexMin:=XmlStateData.RepeatIndex;
          XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;

          XmlStateData.RepeatInfoFine.RepeatNumber:=0;
          XmlStateData.RepeatInfoSegno.RepeatNumber:=0;
          XmlStateData.RepeatInfoCoda.RepeatNumber:=0;
          XmlStateData.RepeatInfoCoda.RepeatStart:=0;

          XmlStateData.XmlTimeIndex:=0;
          XmlStateData.XmlTimeIndexMaxInMeasure:=0;

          XmlStateData.BeforeFirstMeasure:=true;
          XmlStateData.Transpose:=0;
          XmlStateData.MeasureTime:=0;
////          XmlStateData.MidiInstrumentMax:=0;
          XmlStateData.MidiInstrumentIndex:=0;
            FillChar(NoteHitCount,sizeof(THitCount),0);
            FillChar(TextHitCount,sizeof(THitCount),0);
            FillChar(MaxHitCount,sizeof(THitCount),0);
          XmlState:=XmlStatePartBegin;
          end;
        end;
      XmlStateScorePartEnd:
        begin
        if Symbol.StateSymbol=XmlStatePartListEnd then
          XmlState:=XmlStatePartListEnd
        else if Symbol.StateSymbol=XmlStateScorePartBegin then
          begin
          XmlStateData.PartId:=Symbol.InfoId;
          XmlState:=XmlStateScorePartBegin;
          end;
        end;
      XmlStateCreditBegin:
        begin
        if Symbol.StateSymbol=XmlStateCreditWordsBegin then
          XmlState:=XmlStateCreditWordsBegin
        else if Symbol.StateSymbol=XmlStateCreditTypeBegin then
          XmlState:=XmlStateCreditTypeBegin
        end;
      XmlStateCreditWordsBegin:
        begin
        if Symbol.StateSymbol=XmlStateCreditWordsEnd then
          begin
          // There should be a title command but if not - use this test instead
          if XmlStateData.SongTitle='' then
{$ifdef FPC}
{$ifdef Darwin}
          XmlStateData.SongTitle:=Symbol.InfoBefore;
{$else}
////          XmlStateData.SongTitle:=Utf8ToAnsi(Symbol.InfoBefore);
          XmlStateData.SongTitle:=(Symbol.InfoBefore);
{$endif}
{$else}
            XmlStateData.SongTitle:=Symbol.InfoBefore;
{$endif}
          if (Pos('Symbol.InfoBefore',XmlStateData.SongTitle)=0) and (XmlStateData.CreditType='composer') then
            XmlStateData.SongTitle:=XmlStateData.SongTitle+' / '+Symbol.InfoBefore;

          XmlState:=XmlStateCreditWordsEnd;
          end;
        end;
      XmlStateCreditWordsEnd:
        begin
        if Symbol.StateSymbol=XmlStateCreditEnd then
          XmlState:=XmlStateCreditEnd
        else if Symbol.StateSymbol=XmlStateCreditWordsBegin then
          XmlState:=XmlStateCreditWordsBegin
        else
          XmlError('Credit end missing');
        end;

      XmlStateCreditTypeBegin:
        begin
        if Symbol.StateSymbol=XmlStateCreditTypeEnd then
          begin
          XmlStateData.CreditType:=Symbol.InfoBefore;
          XmlState:=XmlStateCreditTypeEnd;
          end;
        end;
      XmlStateCreditTypeEnd:
        begin
        if Symbol.StateSymbol=XmlStateCreditWordsBegin then
          XmlState:=XmlStateCreditWordsBegin
        else if Symbol.StateSymbol=XmlStateCreditEnd then
          XmlState:=XmlStateCreditEnd
        else
          XmlError('CreditType end missing');
        end;

      XmlStateCreditEnd:
        begin
        if Symbol.StateSymbol=XmlStatePartListBegin then
          XmlState:=XmlStatePartListBegin
        else if Symbol.StateSymbol=XmlStateCreditBegin then
          XmlState:=XmlStateCreditBegin
        else if Symbol.StateSymbol=XmlStatePartBegin then
          begin
          XmlStateData.PartId:=Symbol.InfoId;
          XmlStateData.PartNumber:=LookUpPartId(XmlStateData.PartId);
          if XmlStateData.PartNumber>=0 then
          XmlStateData.CurrentVolume:=XmlStateData.Volume[XmlStateData.TrackNumber];
          NoteInfoGraceIndex:=1;
          NoteInfoGraceIndexStart:=1;

          XmlStateData.CurrentVolumeStart:=0;
          XmlStateData.VoltaText:='';
            XmlStateData.VoltaTextRepeat:='';
          XmlStateData.VerseNumber:=1;
          XmlStateData.VerseStart:=GetFilePos(DataIn);
          XmlStateData.VerseStartLineNumber:=XmlLineNumber;
          XmlStateData.VerseStartMeasureNumber:=XmlStateData.MeasureNumber;
          XmlStateData.StaffNumber:=1;
          XmlStateData.RepeatInfoCurrent.RepeatStart:=GetFilePos(DataIn);
          XmlStateData.RepeatInfoCurrent.RepeatBeginOpen:=false;
          XmlStateData.RepeatInfoCurrent.RepeatStartLineNumber:=XmlLineNumber;
          XmlStateData.RepeatInfoCurrent.RepeatStartState:=XmlStateRepeatPartBegin;
          XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;
            XmlStateData.RepeatInfoCurrent.RepeatNoteNumber:=XmlStateData.NoteNumber;

          XmlStateData.RepeatIndex:=1;
          PushRepeat(XmlStateData.RepeatInfoCurrent);
          XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;

          XmlStateData.RepeatInfoFine.RepeatNumber:=0;
          XmlStateData.RepeatInfoSegno.RepeatNumber:=0;
          XmlStateData.RepeatInfoCoda.RepeatNumber:=0;
          XmlStateData.RepeatInfoCoda.RepeatStart:=0;


          XmlStateData.XmlTimeIndex:=0;
          XmlStateData.XmlTimeIndexMaxInMeasure:=0;

          XmlStateData.BeforeFirstMeasure:=true;
          XmlStateData.Transpose:=0;
          XmlStateData.MeasureTime:=0;
          XmlStateData.MidiInstrumentIndex:=0;
            FillChar(NoteHitCount,sizeof(THitCount),0);
            FillChar(TextHitCount,sizeof(THitCount),0);
            FillChar(MaxHitCount,sizeof(THitCount),0);
          XmlState:=XmlStatePartBegin;
          end;
        end;
      XmlStatePartBegin:
        begin
          if XmlStateData.PartId='' then
          begin
            if (XmlStateData.PartNumberMax<Tracks) then
              begin
          XmlStateData.PartId:='PartNo'+IntToStr(XmlStateData.PartNumberMax);
          XmlStateData.PartMap[XmlStateData.PartNumberMax]:=XmlStateData.PartId;
          inc(XmlStateData.PartNumberMax);
          end;
            end;
        //// Error or warning and stop or not????
        if XmlStateData.PartId='' then
          begin
            XmlError(LinguaTextPartNumber+XmlStateData.PartId+LinguaTextIsMissing);
          XmlStateData.PartNumber:=0;
          end
        else
            begin
            XmlStateData.PartNumber:=LookupPartId(XmlStateData.PartId);
            if XmlStateData.PartNumber<0 then
              begin
              //// Error or warning and stop or not????
              XmlStateData.PartMap[XmlStateData.PartNumberMax]:=XmlStateData.PartId;
              inc(XmlStateData.PartNumberMax);
              end;

            XmlStateData.PartNumber:=LookupPartId(XmlStateData.PartId);
            if XmlStateData.PartNumber<0 then
              begin
              XmlError(LinguaTextPartNumber+XmlStateData.PartId+' was not found');
              XmlStateData.PartNumber:=0;
              end;
            end;

        // At first measure begin, set part/track data
        if Symbol.StateSymbol=XmlStateMeasureBegin then
          begin
          XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
          if IsNumber(Symbol.InfoNumber) then
          XmlStateData.MeasureNumber:=StringToInt(Symbol.InfoNumber);
          SetSegnoCodaCapo;
          // Before part P1: Write Midi Header (index from 0)
          if XmlStateData.BeforeFirstPart then
            begin
            XmlStateData.FirstPartNumber:=XmlStateData.PartNumber;
            XmlStateData.BeforeFirstPart:=false;
            XmlMidiHeader(XmlStateData,MidiData);
            XmlMidiChangeTrack(XmlStateData,MidiData);
            end
          else
            begin
            XmlMidiChangeTrack(XmlStateData,MidiData)
            end;
          // If measure number is one, then previous notes shall be expanded
          // in order to get measures aligned with midi
          if StringToInt(Symbol.InfoNumber)>0 then
            begin
            XmlStateData.BeforeFirstMeasure:=false;
            j:=XmlStateData.XmlTimeIndex mod XmlStateData.Divisions;
            for i:=XmlStateData.XmlTimeIndex-1 downto 0 do
              begin
              PNoteInfo^[i+j]:=PNoteInfo^[i];
              end;
            end;
          XmlStateData.MeasureTime:=XmlStateData.XmlTimeIndex;

          if Symbol.InfoImplicit<>'"yes"' then
            begin
            XmlStateData.MeasureNumber:=StringToInt(Symbol.InfoNumber)
            end
          else
            XmlStateData.MeasureNumber:=0;

          XmlState:=XmlStateMeasureBegin;
          end;
        end;

      // Special version of PartBegin used when a repeat brings the
      // fileposition back to part-start.
      XmlStateRepeatPartBegin:
        begin
        // At first measure begin, set part/track data
        if Symbol.StateSymbol=XmlStateMeasureBegin then
          begin
          XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
          if IsNumber(Symbol.InfoNumber) then
          XmlStateData.MeasureNumber:=StringToInt(Symbol.InfoNumber);
          SetSegnoCodaCapo;
          // If measure number is one, then previous notes shall be expanded
          // in order to get measures aligned with midi
////          XmlStateData.MeasureNumber:=0;
          if IsNumber(Symbol.InfoNumber) then
          XmlStateData.MeasureNumber:=StringToInt(Symbol.InfoNumber);
          if StringToInt(Symbol.InfoNumber)>0 then
            begin
{
            XmlStateData.BeforeFirstMeasure:=false;
            j:=XmlStateData.XmlTimeIndex mod XmlStateData.Divisions;
            for i:=XmlStateData.XmlTimeIndex-1 downto 0 do
              begin
              PNoteInfo^[i+j]:=PNoteInfo^[i];
              end;
}
            end;
          XmlStateData.MeasureTime:=XmlStateData.XmlTimeIndex;
          XmlState:=XmlStateMeasureBegin;
          end;
        end;

      XmlStateBarlineEnd,
      XmlStateBackupEnd,
      XmlStateForwardEnd,
      XmlStateDirectionEnd,
      XmlStateMeasureBegin:
        begin
        if Symbol.StateSymbol=XmlStateAttributesBegin then
          XmlState:=XmlStateAttributesBegin
        else if Symbol.StateSymbol=XmlStateNoteBegin then
          begin
          if Symbol.InfoVibrato<>'' then
            begin
            p:=Pos(',',Symbol.InfoVibrato);
            XmlStateData.Attribute:=AttributeVibrato;
            XmlStateData.AttributeParm1:=StringToInt(copy(Symbol.InfoVibrato,1,p));
            XmlStateData.AttributeParm2:=StringToInt(
                          copy(Symbol.InfoVibrato,p,Length(Symbol.InfoVibrato)-p));
            end;
          XmlState:=XmlStateNoteBegin
          end
        else if Symbol.StateSymbol=XmlStateDirectionBegin then
          XmlState:=XmlStateDirectionBegin

        else if Symbol.StateSymbol=XmlStateBackupBegin then
          XmlState:=XmlStateBackupBegin

        else if Symbol.StateSymbol=XmlStateForwardBegin then
          XmlState:=XmlStateForwardBegin

        else if Symbol.StateSymbol=XmlStateMeasureEnd then
          begin
          // A missing fermata must be noted to avoid missalligment
          if FermatInMeasureAll[XmlStateData.MeasureNumber] and
          (not XmlStateData.FermatInMeasurePart[XmlStateData.MeasureNumber]) then
             inc(XmlStateData.FermataMissing);
          XmlState:=XmlStateMeasureEnd;
          CheckSegnoCodaCapo;
          end
        else if Symbol.StateSymbol=XmlStateBarlineBegin then
          begin
          XmlState:=XmlStateBarlineBegin
          end;
{        else
          XmlError(LinguaTextUnknownDefinition+Symbol.InfoName+') in "measure"');}
        end;

      XmlStateDirectionBegin:
        begin
        if ((Length(XmlStateData.VoltaText)=0) or
              (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaText)>0))
        then
          begin
        if Symbol.StateSymbol=XmlStateDynamicsP then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=60;
          end
        else if Symbol.StateSymbol=XmlStateDynamicsPP then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=40;
          end
        else if Symbol.StateSymbol=XmlStateDynamicsPPP then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=20;
          end

        else if Symbol.StateSymbol=XmlStateDynamicsMP then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=80;
          end
        else if Symbol.StateSymbol=XmlStateDynamicsMF then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=90;
          end

        else if Symbol.StateSymbol=XmlStateDynamicsF then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=100;
          end
        else if Symbol.StateSymbol=XmlStateDynamicsFF then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=110;
          end
        else if Symbol.StateSymbol=XmlStateDynamicsFFF then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=120;
          end
        else if Symbol.StateSymbol=XmlStateDynamicsSFZ then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=127;
          end
        else if Symbol.StateSymbol=XmlStateDynamicsSF then
          begin
          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
          XmlStateData.CurrentVolume:=127;
          end
        else if Symbol.StateSymbol=XmlStateWedge then
          begin
          if Symbol.InfoTypeType='"crescendo"' then
            begin
            for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
              PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
            XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
            XmlStateData.CurrentVolumeUp:=true;
            end
          else if Symbol.InfoTypeType='"diminuendo"' then
            begin
            for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
              PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
            XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
            XmlStateData.CurrentVolumeUp:=false;
            end
          else if Symbol.InfoTypeType='"stop"' then
            begin
            newvolume:=XmlStateData.CurrentVolume;
            for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
              begin
              if XmlStateData.CurrentVolumeUp then
                begin // Crescendo. Increase volume to midle of old and max volume
                newvolume:=MidiVolumeMax-(MidiVolumeMax-XmlStateData.CurrentVolume) div 2;
                PNoteInfo^[i].Volume:=
                  XmlStateData.CurrentVolume+((newvolume-XmlStateData.CurrentVolume)*
                                     (i-XmlStateData.CurrentVolumeStart)) div
                     ((3*(XmlStateData.XmlTimeIndex-XmlStateData.CurrentVolumeStart+3)) div 4)+
                     ((newvolume-XmlStateData.CurrentVolume) div 4);
                end
              else
                begin // Diminuendo. Decrease volume to 3/4 og current volume
                newvolume:=(3*XmlStateData.CurrentVolume) div 4;
                PNoteInfo^[i].Volume:=
                  XmlStateData.CurrentVolume+((newvolume-XmlStateData.CurrentVolume)*
                                     (i-XmlStateData.CurrentVolumeStart)) div
                     ((3*(XmlStateData.XmlTimeIndex-XmlStateData.CurrentVolumeStart+3)) div 4)+
                     ((newvolume-XmlStateData.CurrentVolume) div 4);
                end
              end;
            XmlStateData.CurrentVolume:=newvolume;
            XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
            end;
          end
        else if Symbol.StateSymbol=XmlStateSound then
          begin
          if Symbol.InfoTempo<>'' then
            begin
            if ((Length(XmlStateData.VoltaText)=0) or
              (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaText)>0))
              then
            XmlStateData.Tempo:=StringToInt(Symbol.InfoTempo);
              XmlStateData.TempoOrg:=XmlStateData.Tempo;
              XmlStateData.Ritardando:=0;
            end
           end;
         end;

         if Symbol.StateSymbol=XmlStateWordsBegin then
            begin
            XmlState:=XmlStateWordsBegin;
            end
        else if Symbol.StateSymbol=XmlStateDirectionEnd then
          begin
          XmlState:=XmlStateDirectionEnd;
           end
        else if Symbol.StateSymbol=XmlStateDirectionTypeEnd then
          begin
          XmlState:=XmlStateDirectionBegin;
          end
         else if Symbol.StateSymbol=XmlStateDirectionTypeBegin then
           begin
////           XmlState:=XmlStateDirectionTypeBegin;
           end
           else if Symbol.StateSymbol=XmlStateDash then
             begin
             if Symbol.InfoTypeType='"stop"' then
               begin
               XmlStateData.Tempo:=XmlStateData.TempoOrg;
               XmlStateData.Ritardando:=0;
               end;
             end
          end;

       XmlStateDirectionTypeBegin:
         begin
         if Symbol.StateSymbol=XmlStateDirectionTypeEnd then
           XmlState:=XmlStateDirectionTypeEnd;
        end;

       XmlStateWordsBegin:
         begin
         if Symbol.StateSymbol=XmlStateWordsEnd then
           begin
           if (LowerCase(Symbol.InfoBefore)='rit.') or
              (Pos('ritard',LowerCase(Symbol.InfoBefore))>0) then
           // One measure of ritardando
             begin
             XmlStateData.Ritardando:=XmlStateData.Divisions*XmlStateData.Beats;
             XmlStateData.TempoOrg:=XmlStateData.Tempo;
           XmlState:=XmlStateDirectionBegin;
             end
           else if Pos('[SON]',Symbol.InfoBefore)>0 then
             begin
             if XmlStateData.MusicSubroutine.RepeatStart>0 then
               begin
               SetFilePosition(DataIn,XmlStateData.MusicSubroutine.RepeatStart,0);
               XmlLineNumber:=XmlStateData.MusicSubroutine.RepeatStartLineNumber;

               //// TEST
               DirectionLineNumber:=XmlLineNumber;

               XmlStateData.MeasureNumber:=XmlStateData.MusicSubroutine.RepeatMeasureNumber;
               XmlState:=XmlStateData.MusicSubroutine.RepeatStartState;
               XmlStateData.NoteNumber:=XmlStateData.MusicSubroutine.RepeatNoteNumber;
               XmlStateData.MusicSubroutine.RepeatStart:=0;
               XmlStateData.RepeatInfoSegno.RepeatNumber:=0;
{
               if XmlStateData.MusicSubroutine.RepeatTempo>0 then
                 begin
                 XmlStateData.Tempo:=XmlStateData.MusicSubroutine.RepeatTempo;
                 XmlStateData.TempoOrg:=XmlStateData.Tempo;
                 end
}
////               if XmlStateData.TempoOrg>0 then XmlStateData.Tempo:=XmlStateData.TempoOrg;
               end
             else
               begin
               XmlState:=XmlStateDirectionBegin;
               end
             end
           else
             begin
           XmlState:=XmlStateDirectionBegin;
         end;
////           XmlState:=XmlStateDirectionBegin;
         end;
         end;

      XmlStateEndingBegin:
        begin
        if Symbol.StateSymbol=XmlStateEndingEnd then
          XmlState:=XmlStateEndingEnd;
        end;

      XmlStateEndingEnd,
      XmlStateBarlineBegin:
        begin
        if Symbol.StateSymbol=XmlStateEndingBegin then
          begin
          if Symbol.InfoTypeType='"start"' then
            XmlStateData.VoltaText:=Symbol.InfoNumber
          else if Symbol.InfoTypeType='"discontinue"' then
              begin
  ////            XmlStateData.RepeatInfoCurrent:=PopRepeat; //// ?????
              XmlStateData.VoltaText:='';
  ////            XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;
              end
          else
              begin
              // Repeat comes later, but must still know the repeat
              XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
            XmlStateData.VoltaText:='';
              end;
          XmlState:=XmlStateEndingBegin;
          end
        else if Symbol.StateSymbol=XmlStateBarlineEnd then
          XmlState:=XmlStateBarlineEnd
        else if Symbol.StateSymbol=XmlStateRepeat then
          begin
          if Symbol.InfoDirection='"forward"' then
            begin // Save info the first time the repeat begin comes
////            if  XmlStateData.RepeatInfoCurrent.RepeatNumber=1 then
              begin      //// qqqqqqqqqqqqqqq
            PushRepeat(XmlStateData.RepeatInfoCurrent);
            XmlStateData.RepeatInfoCurrent.RepeatStart:=GetFilePos(DataIn);
            XmlStateData.RepeatInfoCurrent.RepeatBeginOpen:=false;
            XmlStateData.RepeatInfoCurrent.RepeatStartLineNumber:=XmlLineNumber;
            XmlStateData.RepeatInfoCurrent.RepeatStartState:=XmlStateBarlineBegin;
            XmlStateData.RepeatInfoCurrent.RepeatBeginOpen:=true;
            XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;
            XmlStateData.RepeatInfoCurrent.RepeatMeasureNumber:=XmlStateData.MeasureNumber; ////
            XmlStateData.RepeatInfoCurrent.RepeatNoteNumber:=XmlStateData.NoteNumber;
            XmlStateData.VoltaText:='';
            XmlStateData.VoltaTextRepeat:='';
            XmlStateData.VerseLast:=0;
            XmlStateData.LyricVerseNumber:=0;
            end
            end
          else
          if Symbol.InfoDirection='"backward"' then
            begin
            XmlStateData.RepeatInfoCurrent.RepeatInMeasure:=true;
            XmlStateData.RepeatInfoCurrent.RepeatBeginOpen:=false;
            // Either no volta(repeat once) or voltas
              if (((XmlStateData.VoltaTextRepeat='') and (XmlStateData.RepeatInfoCurrent.RepeatNumber=1)) or
                  ((XmlStateData.RepeatInfoCurrent.RepeatNumber<XmlStateData.VoltaMax) and
                  (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaTextRepeat)>0))) and
                (not ((DirectionFine in MeasureDirections[XmlStateData.MeasureNumber]) and
                     (XmlStateData.RepeatInfoCapo.RepeatNumber>1)))
              then
              begin  // Repeat - set position back
              SetFilePosition(DataIn,XmlStateData.RepeatInfoCurrent.RepeatStart,0);
              ReadXmlBufferIndex:=0;
              XmlDataInCount:=0;

              // A missing fermata must be noted to avoid missalligment
              if FermatInMeasureAll[XmlStateData.MeasureNumber] and
                (not XmlStateData.FermatInMeasurePart[XmlStateData.MeasureNumber]) then
                inc(XmlStateData.FermataMissing);

              XmlLineNumber:=XmlStateData.RepeatInfoCurrent.RepeatStartLineNumber;
              XmlState:=XmlStateData.RepeatInfoCurrent.RepeatStartState;
                XmlStateData.NoteNumber:=XmlStateData.RepeatInfoCurrent.RepeatNoteNumber;
              XmlStateData.MeasureNumber:=XmlStateData.RepeatInfoCurrent.RepeatMeasureNumber;
              inc(XmlStateData.RepeatInfoCurrent.RepeatNumber);
              XmlStateData.MeasureTime:=XmlStateData.MeasureTime+
               (XmlStateData.Divisions*XmlStateData.Beats*
                4 div (XmlStateData.BeatType));
                XmlStateData.VoltaTextRepeat:='';
              XmlStateData.VoltaText:='';
              XmlStateData.Tempo:=XmlStateData.TempoOrg;
              XmlStateData.Ritardando:=0;
              end
            else
              begin
              // If continue then reset to first repeat
                if ((XmlStateData.VoltaText='') and
                   ((XmlStateData.RepeatInfoCurrent.RepeatNumber<XmlStateData.VoltaMax)
                   and (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),
                      XmlStateData.VoltaTextRepeat)>0)))
                   or ((XmlStateData.VoltaTextRepeat='')) then
                begin
                  XmlStateData.RepeatInfoCurrent:=PopRepeat;
                XmlStateData.RepeatInfoCurrent.RepeatInMeasure:=true;
                ////XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;
                  XmlStateData.VoltaTextRepeat:='';
                  end
                else
                  begin
                  XmlStateData.VoltaTextRepeat:='';
////                inc(XmlStateData.RepeatInfoCurrent.RepeatNumber);
                end;
              end;
            end
          end
        else if Symbol.StateSymbol=XmlStateEnding then
          begin
          if Symbol.InfoTypeType='"start"' then
              begin
              if VoltaDiscontinueInMeasure[XmlStateData.MeasureNumber] then
                begin
                XmlStateData.VoltaText:='';
                XmlStateData.VoltaTextRepeat:='';
                XmlStateData.RepeatInfoCurrent:=PopRepeat; //// FLYT TIL STARTEN AF VOLTA:N
                end
              else
                begin
            XmlStateData.VoltaText:=Symbol.InfoNumber;
                XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
{
                for i:=1 to 10 do
                  begin
                  if (Pos(IntToStr(i),XmlStateData.VoltaTextRepeat)>0) then
                  if XmlStateData.VoltaMax<i then XmlStateData.VoltaMax:=i;
                  end;
}                  
                end;
              end
            else if Symbol.InfoTypeType='"stop"' then
              begin
              XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
              XmlStateData.VoltaText:='';
            end
          else if Symbol.InfoTypeType='"discontinue"' then
            begin
              XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
            XmlStateData.VoltaText:='';
            end
              end;
        end;


      XmlStateAttributesBegin,
      XmlStateKeyEnd,
      XmlStateTimeEnd,
      XmlStateClefEnd,
      XmlStateTransposeEnd,
      XmlStateDivisionsEnd:
        begin
        if Symbol.StateSymbol=XmlStateDivisionsBegin then
          XmlState:=XmlStateDivisionsBegin
        else if Symbol.StateSymbol=XmlStateKeyBegin then
          XmlState:=XmlStateKeyBegin
        else if Symbol.StateSymbol=XmlStateTimeBegin then
          begin
          XmlStateData.NoOfTimeSignatures:=0;
          XmlState:=XmlStateTimeBegin
          end
        else if Symbol.StateSymbol=XmlStateClefBegin then
          XmlState:=XmlStateClefBegin
        else if Symbol.StateSymbol=XmlStateTransposeBegin then
          XmlState:=XmlStateTransposeBegin
        else if Symbol.StateSymbol=XmlStateAttributesEnd then
          XmlState:=XmlStateAttributesEnd
////        else
////          XmlError('Unknown definition ('+Symbol.InfoName+') in "midi-instrument"');
        end;

      XmlStateDivisionsBegin:
        begin
        if Symbol.StateSymbol=XmlStateDivisionsEnd then
          begin
          XmlStateData.Divisions:=StringToInt(Symbol.InfoBefore);
          //// BRUGES MULTFACTOR ????
          if XmlStateData.Divisions<>0 then
////          XmlStateData.MultFactor:=10*MultFactorDefault*
////                  (XmlStateData.CommonMultiplicator div XmlStateData.CommonDivisor);
          XmlState:=XmlStateDivisionsEnd;
          end;
        end;
      XmlStateKeyBegin:
        begin
        if Symbol.StateSymbol=XmlStateFifthsBegin then
          XmlState:=XmlStateFifthsBegin

        else if Symbol.StateSymbol=XmlStateKeyEnd then
          XmlState:=XmlStateKeyEnd;
        end;
      XmlStateFifthsBegin:
        begin
        if Symbol.StateSymbol=XmlStateFifthsEnd then
          begin
          XmlStateData.Fifths:=Symbol.InfoBefore;

          XmlState:=XmlStateFifthsEnd;
          end;
        end;
      XmlStateFifthsEnd:
        begin
        if Symbol.StateSymbol=XmlStateModeBegin then
          XmlState:=XmlStateModeBegin
        else if Symbol.StateSymbol=XmlStateKeyEnd then
          XmlState:=XmlStateKeyEnd
        end;
      XmlStateModeBegin:
        begin
        if Symbol.StateSymbol=XmlStateModeEnd then
          begin
          XmlStateData.Mode:=Symbol.InfoBefore;
          XmlState:=XmlStateModeEnd;
          end;
        end;
      XmlStateModeEnd:
        begin
        if Symbol.StateSymbol=XmlStateKeyEnd then
          begin
          XmlState:=XmlStateKeyEnd;
          end;
        end;

      XmlStateTimeBegin:
        begin
        if Symbol.StateSymbol=XmlStateBeatsBegin then
          begin
          inc(XmlStateData.NoOfTimeSignatures);
          XmlState:=XmlStateBeatsBegin;
          end
        else if Symbol.StateSymbol=XmlStateSenzaMisura then
          begin
          ////
          end
        else if Symbol.StateSymbol=XmlStateTimeEnd then
          begin
          XmlState:=XmlStateTimeEnd;
          end


        end;
      XmlStateBeatsBegin:
        begin
        if Symbol.StateSymbol=XmlStateBeatsEnd then
          begin
          XmlStateData.BeatsList[XmlStateData.NoOfTimeSignatures]:=AddToInt(Symbol.InfoBefore);
          XmlState:=XmlStateBeatsEnd;
          end;
        end;
      XmlStateBeatsEnd:
        begin
        if Symbol.StateSymbol=XmlStateBeatTypeBegin then
          begin
          XmlState:=XmlStateBeatTypeBegin;
          end;
        end;
      XmlStateBeatTypeBegin:
        begin
        if Symbol.StateSymbol=XmlStateBeatTypeEnd then
          begin
          XmlStateData.BeatTypeList[XmlStateData.NoOfTimeSignatures]:=StringToInt(Symbol.InfoBefore);
          XmlState:=XmlStateBeatTypeEnd;
          end;
        end;
      XmlStateBeatTypeEnd:
        begin
        if Symbol.StateSymbol=XmlStateBeatsBegin then
          begin
          inc(XmlStateData.NoOfTimeSignatures);
          XmlState:=XmlStateBeatsBegin;
          end
        else if Symbol.StateSymbol=XmlStateTimeEnd then
          begin
          ComputeTimeSignature;
          XmlState:=XmlStateTimeEnd;
          end;
        end;

      XmlStateTransposeBegin:
        begin
        if Symbol.StateSymbol=XmlStateChromaticBegin then
          XmlState:=XmlStateChromaticBegin;
        end;
      XmlStateChromaticbegin:
        begin
        if Symbol.StateSymbol=XmlStateChromaticEnd then
          begin
          XmlStateData.Transpose:=StringToInt(Symbol.InfoBefore);
          XmlState:=XmlStateChromaticEnd;
          end;
        end;
      XmlStateChromaticEnd:
        begin
        if Symbol.StateSymbol=XmlStateTransposeEnd then
          XmlState:=XmlStateTransposeEnd;;
        end;



      XmlStateClefBegin:
        begin
        if Symbol.StateSymbol=XmlStateSignBegin then
          XmlState:=XmlStateSignBegin;
        end;
      XmlStateSignBegin:
        begin
        if Symbol.StateSymbol=XmlStateSignEnd then
          begin
          XmlStateData.Sign:=Symbol.InfoBefore;
          XmlState:=XmlStateSignEnd;
          end;
        end;
      XmlStateSignEnd:
        begin
        if Symbol.StateSymbol=XmlStateLineBegin then
          begin
          XmlState:=XmlStateLineBegin;
          end
        else if Symbol.StateSymbol=XmlStateClefEnd then
          begin
          XmlState:=XmlStateClefEnd;

          end
        end;
      XmlStateLineBegin:
        begin
        if Symbol.StateSymbol=XmlStateLineEnd then
          begin
          // Line end is part of <clef>-symbol, p.t. not used,
          // since there is no corresponfing Midi-clef event.
          XmlStateData.Line:=Symbol.InfoBefore;   //// ????
          XmlState:=XmlStateLineEnd;
          end;
        end;
      XmlStateLineEnd:
        begin
        if Symbol.StateSymbol=XmlStateClefEnd then
          begin
          XmlState:=XmlStateClefEnd;   //// NEJ !!!!
          end;
        end;

      XmlStateAttributesEnd:
        begin
        if Symbol.StateSymbol=XmlStateNoteBegin then
          begin
          if Symbol.InfoVibrato<>'' then
            begin
            p:=Pos(',',Symbol.InfoVibrato);
            XmlStateData.Attribute:=AttributeVibrato;
            XmlStateData.AttributeParm1:=StringToInt(copy(Symbol.InfoVibrato,1,p));
            XmlStateData.AttributeParm2:=StringToInt(
                          copy(Symbol.InfoVibrato,p,Length(Symbol.InfoVibrato)-p));
            end;
          XmlState:=XmlStateNoteBegin;
          end
        else if Symbol.StateSymbol=XmlStateDirectionBegin then
          begin
          XmlState:=XmlStateDirectionBegin
          end
        else if Symbol.StateSymbol=XmlStateBarlineBegin then
          begin
          XmlState:=XmlStateBarlineBegin
          end
        else if Symbol.StateSymbol=XmlStateBackupBegin then
          XmlState:=XmlStateBackupBegin
        else if Symbol.StateSymbol=XmlStateForwardBegin then
          XmlState:=XmlStateForwardBegin
        else if Symbol.StateSymbol=XmlStateMeasureEnd then
          begin
          // A missing fermata must be noted
          if FermatInMeasureAll[XmlStateData.MeasureNumber] and
          (not XmlStateData.FermatInMeasurePart[XmlStateData.MeasureNumber]) then
             inc(XmlStateData.FermataMissing);
          XmlState:=XmlStateMeasureEnd;
          CheckSegnoCodaCapo;
          end
        else
          // Fuglin i ....
          ////XmlError('No notes found in XML file');
        end;

      XmlStateGrace:
        begin
        // Ignore the note  //// Not used
        if Symbol.StateSymbol=XmlStateNoteEnd then
        XmlState:=XmlStateNoteEnd;
        end;

      XmlStateStaffNotesEnd,  
      XmlStateRestEnd,
      XmlStateNoteBegin:
        begin
        XmlStateData.TieStart:=false;
          XmlStateData.Fermata:=false;
        XmlStateData.Staccato:=false;
        XmlStateData.InfoId:=Symbol.InfoId;
        XmlStateData.MidiChannelIndex:=XmlStateData.MidiChannel[XmlStateData.PartNumber];

        XmlStateData.Chord:=false;
        XmlStateData.Lyric:='';
        XmlStateData.LyricVerseNumber:=0;
        if Symbol.StateSymbol=XmlStateChord then
          begin
          XmlStateData.Chord:=true;
          XmlState:=XmlStateChord;
          end
        else if Symbol.StateSymbol=XmlStateGrace then
          begin
          XmlStateData.NoteIsGraceNote:=true;
          end
        else if Symbol.StateSymbol=XmlStateStaffBegin then
          begin
          XmlState:=XmlStateStaffNotesBegin;
          end
        else if Symbol.StateSymbol=XmlStateCue then
          begin
          //// Ignore
          Symbol.StateSymbol:=XmlStateCue;
          end
        else if Symbol.StateSymbol=XmlStatePitchBegin then
          begin
          XmlState:=XmlStatePitchBegin;
          end
        else if Symbol.StateSymbol=XmlStateUnpitchedBegin then
          begin
          XmlState:=XmlStateUnpitchedBegin;
          end
        else if Symbol.StateSymbol=XmlStateRest then
          begin
          XmlStateData.NotePause:=true;
          XmlStateData.Duration:=0; //// XmlStateData.Divisions;
          XmlState:=XmlStateRest;
          end
        else if Symbol.StateSymbol=XmlStateRestBegin then
          begin
          XmlState:=XmlStateRestBegin;
          end
        else if Symbol.StateSymbol=XmlStateNoteEnd then
          begin
          XmlState:=XmlStateNoteEnd;
          end
        else
////          XmlError('Xml Error: after <note> no pitch nor pause');
          ;
        end;

      XmlStateRestBegin:
        if Symbol.StateSymbol=XmlStateRestEnd then
          begin
          // View Teleman.xml about this strange <rest> definition
          XmlState:=XmlStateRest;
          end;

      XmlStateStaffNotesBegin:
        if Symbol.StateSymbol=XmlStateStaffEnd then
          begin
          XmlStateData.StaffNumber:=StringToInt(Symbol.InfoBefore);
          XmlState:=XmlStateStaffNotesEnd;
          end;

      XmlStateStaffDirectionsBegin:
        if Symbol.StateSymbol=XmlStateStaffEnd then
          begin
          XmlStateData.StaffNumber:=StringToInt(Symbol.InfoBefore);
          XmlState:=XmlStateStaffDirectionsEnd;
          end;

      XmlStateStaffDurationBegin:
        if Symbol.StateSymbol=XmlStateStaffEnd then
          begin
          XmlStateData.StaffNumber:=StringToInt(Symbol.InfoBefore);
          XmlState:=XmlStateStaffDurationEnd;
          end;

      XmlStateChord:
        begin
        if Symbol.StateSymbol=XmlStatePitchBegin then
          begin
          XmlState:=XmlStatePitchBegin;
          end
        else if Symbol.StateSymbol=XmlStateRest then
          begin
          XmlState:=XmlStateRest;
          end
        else if Symbol.StateSymbol=XmlStateUnpitchedBegin then
          begin
          XmlState:=XmlStateUnpitchedBegin;
          end
        else
          XmlError(LinguaTextXmlErrorAfterChordNoPitchNorPause);
        end;

      XmlStateUnpitchedBegin:
        begin
        if Symbol.StateSymbol=XmlStateDisplayStepBegin then
          begin
          XmlStateData.Alter:='0';
          XmlStateData.NotePause:=false;
          XmlState:=XmlStateDisplayStepBegin;
          end
        end;
      XmlStateDisplayStepBegin:
        begin
        if Symbol.StateSymbol=XmlStateDisplayStepEnd then
          begin
          XmlState:=XmlStateDisplayStepEnd;
          XmlStateData.Step:=Symbol.InfoBefore;
          end
        end;
      XmlStateDisplayStepEnd:
        begin
        if Symbol.StateSymbol=XmlStateDisplayAlterBegin then
          begin
          XmlState:=XmlStateDisplayAlterBegin;
          end
        else if Symbol.StateSymbol=XmlStateDisplayOctaveBegin then
          begin
          XmlState:=XmlStateDisplayOctaveBegin;
          end
        end;
      XmlStateDisplayAlterBegin:
        begin
        if Symbol.StateSymbol=XmlStateDisplayAlterEnd then
          begin
          XmlState:=XmlStateDisplayAlterEnd;
          XmlStateData.Alter:=Symbol.InfoBefore;
          end
        end;
      XmlStateDisplayAlterEnd:
        begin
        if Symbol.StateSymbol=XmlStateDisplayOctaveBegin then
          begin
          XmlState:=XmlStateDisplayOctaveBegin;
          XmlStateData.Octave:=Symbol.InfoBefore;
          end
        end;
      XmlStateDisplayOctaveBegin:
        begin
        if Symbol.StateSymbol=XmlStateDisplayOctaveEnd then
          begin
          XmlState:=XmlStateDisplayOctaveEnd;
          XmlStateData.Octave:=Symbol.InfoBefore;
          end
        end;
      XmlStateDisplayOctaveEnd:
        begin
        if Symbol.StateSymbol=XmlStateUnpitchedEnd then
          begin
          XmlState:=XmlStateUnpitchedEnd;
          end
        end;
      XmlStateUnpitchedEnd:
        begin
        if Symbol.StateSymbol=XmlStateDurationBegin then
          begin
          XmlState:=XmlStateDurationPitchBegin;
          end
        end;

      XmlStatePitchBegin:
        begin
        if Symbol.StateSymbol=XmlStateStepBegin then
          begin
          XmlStateData.Alter:='0';
          XmlStateData.NotePause:=false;
          XmlState:=XmlStateStepBegin;
          end
        end;
      XmlStateStepBegin:
        begin
        if Symbol.StateSymbol=XmlStateStepEnd then
          begin
          XmlState:=XmlStateStepEnd;
          XmlStateData.Step:=Symbol.InfoBefore;
          end
        end;
      XmlStateStepEnd:
        begin
        if Symbol.StateSymbol=XmlStateAlterBegin then
          begin
          XmlState:=XmlStateAlterBegin;
          end
        else if Symbol.StateSymbol=XmlStateOctaveBegin then
          begin
          XmlState:=XmlStateOctaveBegin;
          end
        end;
      XmlStateAlterBegin:
        begin
        if Symbol.StateSymbol=XmlStateAlterEnd then
          begin
          XmlState:=XmlStateAlterEnd;
          XmlStateData.Alter:=Symbol.InfoBefore;
          end
        end;
      XmlStateAlterEnd:
        begin
        if Symbol.StateSymbol=XmlStateOctaveBegin then
          begin
          XmlState:=XmlStateOctaveBegin;
          end
        else if Symbol.StateSymbol=XmlStatePitchEnd then
          begin
          XmlState:=XmlStatePitchEnd;
          end
        end;
      XmlStateOctaveBegin:
        begin
        if Symbol.StateSymbol=XmlStateOctaveEnd then
          begin
          XmlState:=XmlStateOctaveEnd;
          XmlStateData.Octave:=Symbol.InfoBefore;
          end
        end;
      XmlStateOctaveEnd:
        begin
        if Symbol.StateSymbol=XmlStatePitchEnd then
          begin
          XmlState:=XmlStatePitchEnd;
          end
        else if Symbol.StateSymbol=XmlStateAlterBegin then
          begin
          XmlState:=XmlStateAlterbegin;
          end
        end;
      XmlStatePitchEnd:
        begin
        if Symbol.StateSymbol=XmlStateDurationBegin then
          begin
          XmlState:=XmlStateDurationPitchBegin;
          end
        else if Symbol.StateSymbol=XmlStateNoteEnd then
          begin
          XmlState:=XmlStateNoteEnd;
          end
        end;
      XmlStateDurationPitchBegin:
        begin
        if Symbol.StateSymbol=XmlStateDurationEnd then
          begin
          XmlStateData.Duration:=(XmlStateData.CommonMultiplicator*StringToInt(Symbol.InfoBefore)) div
                                 (XmlStateData.Divisions);

          if (XmlStateData.Duration>100) or (XmlStateData.Duration<=0) then
             XmlStateData.ActualNotes:='4';   //// not used !!!!

          XmlStateData.ActualNotes:='4';
          XmlStateData.NormalNotes:='4';
          XmlState:=XmlStateDurationPitchEnd;
          end
        end;

      XmlStateStaffDirectionsEnd,
      XmlStateDurationPitchEnd,
      XmlStateStemEnd,
      XmlStateLyricEnd,
      XmlStateTimeModificationEnd:

        begin
        if Symbol.StateSymbol=XmlStateStemBegin then
          begin
          XmlState:=XmlStateStemBegin;
          end
        else if Symbol.StateSymbol=XmlStateLyricBegin then
          begin
           XmlStateData.LyricVerseNumber:=StringToInt(Symbol.InfoNumber);
          XmlStateData.LyricTimeOnly:=Symbol.InfoTimeOnly;
          // If number is missing - then = 1
          if XmlStateData.LyricVerseNumber<=1 then XmlStateData.LyricVerseNumber:=1;
          if XmlStateData.VerseLast<XmlStateData.LyricVerseNumber then
            XmlStateData.VerseLast:=XmlStateData.LyricVerseNumber;

          XmlState:=XmlStateLyricBegin;
          end
        else if Symbol.StateSymbol=XmlStateNoteBegin then
          begin
          if Symbol.InfoVibrato<>'' then
            begin
            p:=Pos(',',Symbol.InfoVibrato);
            XmlStateData.Attribute:=AttributeVibrato;
            XmlStateData.AttributeParm1:=StringToInt(copy(Symbol.InfoVibrato,1,p));
            XmlStateData.AttributeParm2:=StringToInt(
                          copy(Symbol.InfoVibrato,p,Length(Symbol.InfoVibrato)-p));
            end;
          XmlState:=XmlStateNoteBegin;
          end
        else if Symbol.StateSymbol=XmlStateNoteEnd then
          begin
          XmlState:=XmlStateNoteEnd;
          end
        else if Symbol.StateSymbol=XmlStateStaffBegin then
          begin
          XmlState:=XmlStateStaffDirectionsBegin;
          end
        else if Symbol.StateSymbol=XmlStateBarlineBegin then
          begin
          XmlState:=XmlStateBarlineBegin
          end
        else if Symbol.StateSymbol=XmlStateNotationsBegin then
          begin
          XmlState:=XmlStateNotationsBegin;
          end
        else if Symbol.StateSymbol=XmlStateArticulationsBegin then
          begin
          XmlState:=XmlStateArticulationsBegin;
          end
        else if Symbol.StateSymbol=XmlStateTie then
          begin
          if Symbol.InfoTypeType='"stop"' then
            XmlStateData.TieStop:=true ////  false?
          else if Symbol.InfoTypeType='"start"' then
            XmlStateData.TieStart:=true
          end
        else if Symbol.StateSymbol=XmlStateBackupBegin then
          begin
          XmlState:=XmlStateBackupBegin
          end
        else if Symbol.StateSymbol=XmlStateForwardBegin then
          begin
          XmlState:=XmlStateForwardBegin
          end
        else if Symbol.StateSymbol=XmlStateTimeModificationBegin then
          begin
          XmlState:=XmlStateTimeModificationBegin;
          end
        else if Symbol.StateSymbol=XmlStateInstrument then
          begin
          XmlStateData.InfoId:=Symbol.InfoId;
          // Change the note if percussion and instrument is found in list
          with XmlStateData do
          if Sign='percussion' then
            begin
            // The percussion instrument is the note value
            for i:=0 to MidiInstrumentMax-1 do
              begin
              //// Name?
              if (MidiInstrument[i]=Symbol.InfoId) and (MidiInstrumentPitch[i] in [0..127]) then
                begin
                Octave:=IntToStr((MidiInstrumentPitch[i] div 12)-1);
                  case MidiInstrumentPitch[i] mod 12 of
                  0: XmlStateData.Step:='C';
                  1: XmlStateData.Step:='C';
                  2: XmlStateData.Step:='D';
                  3: XmlStateData.Step:='D';
                  4: XmlStateData.Step:='E';
                  5: XmlStateData.Step:='F';
                  6: XmlStateData.Step:='F';
                  7: XmlStateData.Step:='G';
                  8: XmlStateData.Step:='G';
                  9: XmlStateData.Step:='A';
                  10: XmlStateData.Step:='A';
                  11: XmlStateData.Step:='B';
                  end;

                  case MidiInstrumentPitch[i] mod 12 of
                  0: XmlStateData.Alter:='0';
                  1: XmlStateData.Alter:='1';
                  2: XmlStateData.Alter:='0';
                  3: XmlStateData.Alter:='1';
                  4: XmlStateData.Alter:='0';
                  5: XmlStateData.Alter:='0';
                  6: XmlStateData.Alter:='1';
                  7: XmlStateData.Alter:='0';
                  8: XmlStateData.Alter:='1';
                  9: XmlStateData.Alter:='0';
                  10: XmlStateData.Alter:='1';
                  11: XmlStateData.Alter:='0';
                  end;
                end;
              end;
            end
          else
            begin
            // Normal notes - the instrument is the instrument value
            // defined by the channel
            for i:=0 to XmlStateData.MidiInstrumentMax-1 do
              begin
              if XmlStateData.MidiInstrument[i]=Symbol.InfoId then
                begin
                if XmlStateData.PartNumber>0 then
                XmlStateData.MidiChannelIndex:=XmlStateData.MidiInstrumentChannel[i]
                else
                XmlStateData.MidiChannelIndex:=XmlStateData.MidiInstrumentChannel[i];
                // All settings depending on channel
                ////ChannelIndex:=XmlStateData.MidiChannel[XmlStateData.PartNumber];
                end;
              end;
            end;
          end
        end;

      XmlStateTimeModificationBegin:
        begin
        if Symbol.StateSymbol=XmlStateActualNotesBegin then
          begin
          XmlState:=XmlStateActualNotesBegin;
          end
        end;
      XmlStateActualNotesBegin:
        begin
        if Symbol.StateSymbol=XmlStateActualNotesEnd then
          begin
          XmlStateData.ActualNotes:=Symbol.InfoBefore;
          XmlState:=XmlStateActualNotesEnd;
          end
        end;
      XmlStateActualNotesEnd:
        begin
        if Symbol.StateSymbol=XmlStateNormalNotesBegin then
          begin
          XmlState:=XmlStateNormalNotesBegin;
          end
        end;

      XmlStateNormalNotesBegin:
        begin
        if Symbol.StateSymbol=XmlStateNormalNotesEnd then
          begin
          XmlStateData.NormalNotes:=Symbol.InfoBefore;
          XmlState:=XmlStateNormalNotesEnd;
          end
        end;

      XmlStateNormalNotesEnd:
        begin
        if Symbol.StateSymbol=XmlStateTimeModificationEnd then
          begin
          XmlState:=XmlStateTimeModificationEnd;
          end
        end;


      XmlStateStemBegin:
        begin
        if Symbol.StateSymbol=XmlStateStemEnd then
          begin
          XmlStateData.Stem:=Symbol.InfoBefore;
          XmlState:=XmlStateStemEnd;
          end
        end;
      XmlStateLyricBegin:
        begin
        XmlStateData.Syllabic:='';
        if Symbol.StateSymbol=XmlStateTextBegin then
          begin
          XmlStateData.Lyric:='';
          XmlState:=XmlStateTextBegin;
          end
        else if Symbol.StateSymbol=XmlStateSyllabicBegin then
          begin
          XmlState:=XmlStateSyllabicBegin;
          end
        else if Symbol.StateSymbol=XmlStateLyricEnd then
          begin
          XmlState:=XmlStateLyricEnd;
          end
        end;
      XmlStateTextBegin:
        begin
        if Symbol.StateSymbol=XmlStateTextEnd then
          begin
////
            if XmlStateData.LyricVerseNumber=
              NoteHitCount[XmlStateData.NoteNumber]+1 then
                begin
                if TextHitCount[XmlStateData.NoteNumber]<=XmlStateData.LyricVerseNumber then
                TextHitCount[XmlStateData.NoteNumber]:=XmlStateData.LyricVerseNumber;
////              XmlStateData.Lyric:=XmlStateData.Lyric;
                end;


             if XmlStateData.LyricVerseNumber>MaxHitCount[XmlStateData.NoteNumber] then
               MaxHitCount[XmlStateData.NoteNumber]:=XmlStateData.LyricVerseNumber;

          if (XmlStateData.LyricVerseNumber=NoteHitCount[XmlStateData.NoteNumber]+1)
             or ((NoteHitCount[XmlStateData.NoteNumber]+1) in
                 StringToVerseNumbers(XmlStateData.LyricTimeOnly)) then
            begin
            if XmlStateData.Syllabic='begin' then
              begin
              XmlStateData.Lyric:=XmlStateData.Lyric+Symbol.InfoBefore+'-'
              end
            else if XmlStateData.Syllabic='middle' then
              begin
              XmlStateData.Lyric:=XmlStateData.Lyric+Symbol.InfoBefore+'-'
              end
            else
              begin
              XmlStateData.Lyric:=XmlStateData.Lyric+Symbol.InfoBefore;
                end
            end;
          Symbol.InfoBefore:='';  
          XmlState:=XmlStateTextEnd;
          end
        end;
      XmlStateTextEnd:
        begin
        if Symbol.StateSymbol=XmlStateLyricEnd then
          begin
          XmlState:=XmlStateLyricEnd;
          end
        else if Symbol.StateSymbol=XmlStateTextBegin then
          XmlState:=XmlStateTextBegin;
        end;

      XmlStateSyllabicBegin:
        begin
        if Symbol.StateSymbol=XmlStateSyllabicEnd then
          begin
          XmlStateData.Syllabic:=Symbol.InfoBefore;
          XmlState:=XmlStateSyllabicEnd;
          end
        end;
      XmlStateSyllabicEnd:
        begin
        if Symbol.StateSymbol=XmlStateTextBegin then
          begin
          XmlState:=XmlStateTextBegin;
          end
        else if Symbol.StateSymbol=XmlStateLyricEnd then
          begin
          XmlState:=XmlStateLyricEnd;
          end
        end;

      XmlStateRest:
        begin
        if Symbol.StateSymbol=XmlStateDurationBegin then
          begin
          XmlStateData.ActualNotes:='4';
          XmlStateData.NormalNotes:='4';
          XmlState:=XmlStateDurationRestBegin;
          end
        else if Symbol.StateSymbol=XmlStateDuration then
          begin
          XmlStateData.ActualNotes:='4';
          XmlStateData.NormalNotes:='4';
          XmlState:=XmlStateDurationRest;
          end
        end;

      XmlStateDurationRest:
        begin
        if Symbol.StateSymbol=XmlStateNoteEnd then
          begin
          XmlStateData.NotePause:=true;
          XmlStateData.Duration:=0;
          XmlState:=XmlStateNoteEnd;
          end;
        end;

      XmlStateOrnamentsEnd,
      XmlStateArticulationsEnd,
      XmlStateNotationsBegin:
        begin
        if Symbol.StateSymbol=XmlStateNotationsEnd then
          begin
          XmlState:=XmlStateNotationsEnd;
          end
        else if Symbol.StateSymbol=XmlStateFermata then
          begin
          // If more staffs - ignore except the first
          if XmlStateData.StaffNumber=1 then
            XmlStateData.Fermata:=true;       ////
            XmlStateData.FermatInMeasurePart[XmlStateData.MeasureNumber]:=true;
          end

        else if Symbol.StateSymbol=XmlStateFermataBegin then
          begin  //// Dropped the ...End etc.
          // If more staffs - ignore except the first
          if XmlStateData.StaffNumber=1 then
            XmlStateData.Fermata:=true;       ////
            XmlStateData.FermatInMeasurePart[XmlStateData.MeasureNumber]:=true;
          end


        else if Symbol.StateSymbol=XmlStateOrnamentsBegin then
          begin
          XmlState:=XmlStateOrnamentsBegin;
          end
        else if Symbol.StateSymbol=XmlStateTechnicalBegin then
          begin
          XmlState:=XmlStateTechnicalBegin;
          end
        else if Symbol.StateSymbol=XmlStateArpeggiate then
          begin
          XmlStateData.Arpeggiate:=true;
          end
        else if Symbol.StateSymbol=XmlStateArticulationsBegin then
          begin
          XmlState:=XmlStateArticulationsBegin;
          end;
        end;

      XmlStateArticulationsBegin:
        begin
        if Symbol.StateSymbol=XmlStateArticulationsEnd then
          begin
          XmlState:=XmlStateArticulationsEnd;
          end
        else if Symbol.StateSymbol=XmlStateStaccato then
          begin
          XmlStateData.Staccato:=true;
          end
        else if Symbol.StateSymbol=XmlStateStaccatissimo then
          begin
          XmlStateData.Staccato:=true;
          end
        else if Symbol.StateSymbol=XmlStateSpiccato then
          begin
          XmlStateData.Staccato:=true;
          end
        else if Symbol.StateSymbol=XmlStateBreathMark then
          begin
          XmlStateData.Fermata:=true;
          end
        else if Symbol.StateSymbol=XmlStateCaesura then
          begin
          XmlStateData.Fermata:=true;
          end
        else if Symbol.StateSymbol=XmlStateScoop then
          begin
          XmlStateData.Attribute:=AttributeScoop;
          end
        else if Symbol.StateSymbol=XmlStatePlop then
          begin
          XmlStateData.Attribute:=AttributePlop;
          end
        else if Symbol.StateSymbol=XmlStateVibrato then
          begin
          XmlStateData.Attribute:=AttributeVibrato;
          XmlStateData.AttributeParm1:=10;
          XmlStateData.AttributeParm2:=100;
          end
        else if Symbol.StateSymbol=XmlStateDoit then
          begin
          XmlStateData.Attribute:=AttributeDoit;
          end
        else if Symbol.StateSymbol=XmlStateFalloff then
          begin
          XmlStateData.Attribute:=AttributeFalloff;
          end
        else if Symbol.StateSymbol=XmlStateBreath then
          begin
          XmlStateData.Attribute:=AttributeBreath;
          end
        else if Symbol.StateSymbol=XmlStateCaesura then
          begin
          XmlStateData.Attribute:=AttributeCaesura;
          end
        end;

      // bbbbbbbbb
      XmlStateTechnicalBegin:
        begin
        if Symbol.StateSymbol=XmlStateBendBegin then
          begin
          XmlState:=XmlStateBendBegin;
          end
        else if Symbol.StateSymbol=XmlStateTechnicalEnd then
          begin
          XmlState:=XmlStateNotationsBegin;
          end
        end;

      XmlStateBendBegin:
        begin
        if Symbol.StateSymbol=XmlStateBendAlterBegin then
          begin
          XmlState:=XmlStateBendAlterBegin;
          end
        else if Symbol.StateSymbol=XmlStateRelease then
          begin
          end
        else if Symbol.StateSymbol=XmlStateWithBar then
          begin
          end
        else if Symbol.StateSymbol=XmlStatePreBend then
          begin
          end
        else if Symbol.StateSymbol=XmlStateBendEnd then
          begin
          XmlState:=XmlStateBendEnd;
          XmlStateData.Attribute:=AttributeBend;
          end
        end;

      XmlStateBendEnd:
        begin
        if Symbol.StateSymbol=XmlStateTechnicalEnd then
          begin
          XmlState:=XmlStateNotationsBegin;
          end
        end;

      XmlStateBendAlterBegin:
        begin
        if Symbol.StateSymbol=XmlStateBendAlterEnd then
          begin
          // One halftone = 1 correspong to $20=32 with offset $40=64
          s:=Symbol.InfoBefore;
          if s[1]='-' then
            begin
            s:=copy(s,2,Length(s)-1);
            XmlStateData.AttributeParm1:=-((StringToInt(s)*NotePitchScale+
                              AlterStringToInt(s)) div 128);
            end
          else
            begin
            XmlStateData.AttributeParm1:=(StringToInt(s)*NotePitchScale+
                              AlterStringToInt(s)) div 128;
            end;
          // Midi allows only bend of 2 halftones
          if XmlStateData.AttributeParm1>63 then XmlStateData.AttributeParm1:=63;
          if XmlStateData.AttributeParm1<-63 then XmlStateData.AttributeParm1:=-63;

          XmlStateData.AttributeParm2:=1;
          XmlState:=XmlStateBendBegin;
          end;
        end;


      XmlStateOrnamentsBegin:
        begin
        if Symbol.StateSymbol=XmlStateOrnamentsEnd then
          begin
          XmlState:=XmlStateOrnamentsEnd;
          end
        else if Symbol.StateSymbol=XmlStateTrillMark then
          begin
          XmlStateData.Attribute:=AttributeTrill;
          end
        else if Symbol.StateSymbol=XmlStateShake then
          begin
          XmlStateData.Attribute:=AttributeTrill;
          end
        else if Symbol.StateSymbol=XmlStateMordent then
          begin
          XmlStateData.Attribute:=AttributeTrill;
          end
        else if Symbol.StateSymbol=XmlStateInvertedMordent then
          begin
          XmlStateData.Attribute:=AttributeTrill;
          end
        else if Symbol.StateSymbol=XmlStateTremolo then
          begin
          XmlStateData.Attribute:=AttributeTrill;
          end
         else if Symbol.StateSymbol=XmlStateTremoloBegin then
          begin
          XmlStateData.Attribute:=AttributeTrill;
          end
        else if Symbol.StateSymbol=XmlStateVibrato then
          begin
          XmlStateData.Attribute:=AttributeVibrato;
          XmlStateData.AttributeParm1:=10;
          XmlStateData.AttributeParm2:=100;
          end
       else if Symbol.StateSymbol=XmlStateSchleifer then
          begin
          XmlStateData.Attribute:=AttributeTrill;
          end
        else if Symbol.StateSymbol=XmlStateWavyLine then
          begin
          XmlStateData.Attribute:=AttributeWavyLine;
          end
        else if Symbol.StateSymbol=XmlStateTurn then
          begin
          XmlStateData.Attribute:=AttributeTurn;
          end
        else if Symbol.StateSymbol=XmlStateDelayedTurn then
          begin
          XmlStateData.Attribute:=AttributeDelayedTurn;
          end
        else if Symbol.StateSymbol=XmlStateInvertedTurn then
          begin
          XmlStateData.Attribute:=AttributeInvertedTurn;
          end
        else if Symbol.StateSymbol=XmlStateVibratoBegin then
          begin
          XmlState:=XmlStateVibratoBegin;
          end
        end;

      XmlStateVibratoBegin:
        begin
        if Symbol.StateSymbol=XmlStateVibratoEnd then
          begin
          XmlStateData.Attribute:=AttributeVibrato;
          XmlStateData.AttributeParm1:=StringToInt(Symbol.InfoBefore)+1;
          XmlStateData.AttributeParm2:=200;

          XmlState:=XmlStateVibratoEnd;
          end
        end;

      XmlStateVibratoEnd:
        begin
        if Symbol.StateSymbol=XmlStateOrnamentsEnd then
          begin
          XmlState:=XmlStateOrnamentsEnd;
          end
        end;

      XmlStateNotationsEnd:
        begin
        if Symbol.StateSymbol=XmlStateNoteEnd then
          begin
          XmlState:=XmlStateNoteEnd;
          end
        else if Symbol.StateSymbol=XmlStateLyricBegin then
          begin
          // Compensate for strange numbers of Finale to adjust with first verse
          XmlStateData.LyricVerseNumber:=StringToInt(Symbol.InfoNumber)-
                      XmlStateData.VerseNumberMapFirst[XmlStateData.PartNumber+1]+1;
          if XmlStateData.VerseLast<XmlStateData.LyricVerseNumber then
            XmlStateData.VerseLast:=XmlStateData.LyricVerseNumber;
          XmlState:=XmlStateLyricBegin;
          end
        else if Symbol.StateSymbol=XmlStateNotationsBegin then
          begin
          XmlState:=XmlStateNotationsBegin;
          end
        else if Symbol.StateSymbol=XmlStateAccidental then
          begin
          XmlState:=XmlState;
          end
        else
          begin
////          XmlError('XML Error: No notes after /notation');
          end;
        end;

      XmlStateDurationRestBegin:
        begin
        if Symbol.StateSymbol=XmlStateDurationEnd then
          begin
          XmlStateData.Duration:=(XmlStateData.CommonMultiplicator*StringToInt(Symbol.InfoBefore)) div
                                 (XmlStateData.Divisions);
////          if (XmlStateData.Duration>100) or (XmlStateData.Duration<0) then
////          XmlState:=XmlStateDurationRestEnd
////          else
          XmlState:=XmlStateDurationRestEnd;
          end
        end;

      XmlStateStaffDurationEnd,
      XmlStateDurationRestEnd:
        begin
        if Symbol.StateSymbol=XmlStateFermata then
          begin
          // If more staffs - ignore except the first
          if XmlStateData.StaffNumber=1 then
            XmlStateData.Fermata:=true;       ////
          XmlStateData.FermatInMeasurePart[XmlStateData.MeasureNumber]:=true;
          end
        else if Symbol.StateSymbol=XmlStateNoteEnd then
          begin
          XmlStateData.NotePause:=true;
          XmlState:=XmlStateNoteEnd;
          end
        else if Symbol.StateSymbol=XmlStateStaffBegin then
          begin
          XmlState:=XmlStateStaffDurationBegin;
          end

        end;

      XmlStateBackupBegin:
        if Symbol.StateSymbol=XmlStateDurationBegin then
          XmlState:=XmlStateDurationBackupBegin;

      XmlStateForwardBegin:
        if Symbol.StateSymbol=XmlStateDurationBegin then
          XmlState:=XmlStateDurationForwardBegin;

      XmlStateDurationBackupBegin:
        begin // Xml command "backup" changes the time (back)
        if (XmlStateData.VoltaText<>'') then
          XmlState:=XmlState;


        if Symbol.StateSymbol=XmlStateDurationEnd then
          begin
          if (XmlStateData.VoltaText='') or
             (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaText)>0) then
            begin
          XmlStateData.TimeChange:=(XmlStateData.CommonMultiplicator*StringToInt(Symbol.InfoBefore)) div XmlStateData.Divisions;
          if XmlStateData.XmlTimeIndex<XmlStateData.TimeChange then
              begin
              ////XmlError('Illegal jump before first note');
              end
            else
              begin
              XmlStateData.XmlTimeIndex:=XmlStateData.XmlTimeIndex-XmlStateData.TimeChange;
              if XmlStateData.XmlTimeIndex>XmlStateData.XmlTimeIndexMaxInMeasure then
                XmlStateData.XmlTimeIndexMaxInMeasure:=XmlStateData.XmlTimeIndex;
              end;
            end;
          XmlState:=XmlStateDurationBackupEnd;
          end
        end;

      XmlStateDurationForwardBegin:
        begin // Xml command "backup" changes the time (forward)      //// backup?
        if Symbol.StateSymbol=XmlStateDurationEnd then
          begin
          if (XmlStateData.VoltaText='') or
             (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaText)>0) then
            begin
          XmlStateData.TimeChange:=(XmlStateData.CommonMultiplicator*StringToInt(Symbol.InfoBefore)) div XmlStateData.Divisions;
          XmlStateData.XmlTimeIndex:=XmlStateData.XmlTimeIndex+XmlStateData.TimeChange;


  if XmlStateData.XmlTimeIndex>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName);   ////




          if XmlStateData.XmlTimeIndex>XmlStateData.XmlTimeIndexMaxInMeasure then
            XmlStateData.XmlTimeIndexMaxInMeasure:=XmlStateData.XmlTimeIndex;
            end;
          XmlState:=XmlStateDurationForwardEnd;
          end
        end;

      XmlStateDurationBackupEnd:
        if Symbol.StateSymbol=XmlStateBackupEnd then
          XmlState:=XmlStateBackupEnd;

      XmlStateDurationForwardEnd:
        if Symbol.StateSymbol=XmlStateForwardEnd then
          XmlState:=XmlStateForwardEnd;

      XmlStateNoteEnd:
        begin
          inc(NoteHitCount[XmlStateData.NoteNumber]);
          inc(XmlStateData.NoteNumber);

          if XmlLineNumber=1913 then
            XmlLineNumber:=XmlLineNumber;
          if XmlLineNumber=1997 then
            XmlLineNumber:=XmlLineNumber;

        // Skip all processing if volta and repeat number <> volta number
        // and also if note is grace note.
        if XmlStateData.NoteIsGraceNote then
          begin
          NoteInfoGrace[NoteInfoGraceIndex].NoteOn:=
               [NoteToMidiPitch(XmlStateData.Step,
                XmlStateData.Octave,XmlStateData.Alter)+XmlStateData.Transpose];
          inc(NoteInfoGraceIndex);
          end
        else if (not XmlStateData.NoteIsGraceNote) and
             ((Length(XmlStateData.VoltaText)=0) or
            (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaText)>0))
          then
          begin
          // Write pause or note to Midi
          if XmlStateData.NotePause then
            begin // Pause
            XmlMidiNotePause(XmlStateData);
            if XmlStateData.NoteIsGraceNote then
              NoteInfoGraceIndexStart:=NoteInfoGraceIndex;
            end
          else
            begin // Note
            if XmlStateData.InfoId='' then
              begin
              XmlMidiNoteOn(XmlStateData);
              NoteInfoGraceIndexStart:=NoteInfoGraceIndex;
              if XmlStateData.NoteIsGraceNote then
                NoteInfoGraceIndexStart:=NoteInfoGraceIndex;
              end
            else if XmlStateData.InfoId=XmlStateData.MidiInstrument[XmlStateData.TrackNumber] then
              begin
              XmlMidiNoteOn(XmlStateData);
              NoteInfoGraceIndexStart:=NoteInfoGraceIndex;
              if XmlStateData.NoteIsGraceNote then
                NoteInfoGraceIndexStart:=NoteInfoGraceIndex;
              end
            else
              begin
              // Remove fermata info, since the note is not played (not this instrument)
              if XmlStateData.Fermata then
                begin
              XmlStateData.Fermata:=false;
                end;
              if XmlStateData.FermataMissing>0 then
                begin ////
                end;
              XmlMidiNoNote(XmlStateData);
              if XmlStateData.NoteIsGraceNote then
                NoteInfoGraceIndexStart:=NoteInfoGraceIndex;
              end;
            end;
          end;

         if XmlState<>XmlStateError then  
         if Symbol.StateSymbol=XmlStateNoteBegin then
          begin
          if Symbol.InfoVibrato<>'' then
            begin
            p:=Pos(',',Symbol.InfoVibrato);
            XmlStateData.Attribute:=AttributeVibrato;
            XmlStateData.AttributeParm1:=StringToInt(copy(Symbol.InfoVibrato,1,p));
            XmlStateData.AttributeParm2:=StringToInt(
                          copy(Symbol.InfoVibrato,p,Length(Symbol.InfoVibrato)-p));
            end;
          XmlState:=XmlStateNoteBegin;
          end
        else if Symbol.StateSymbol=XmlStateBackupBegin then
          begin
          XmlState:=XmlStateBackupBegin
          end
        else if Symbol.StateSymbol=XmlStateForwardBegin then
          begin
          XmlState:=XmlStateForwardBegin
          end
        else if Symbol.StateSymbol=XmlStateBarlineBegin then
          begin
          XmlState:=XmlStateBarlineBegin
          end
        else if Symbol.StateSymbol=XmlStateMeasureEnd then
          begin
          // A missing fermata must be noted
          if FermatInMeasureAll[XmlStateData.MeasureNumber] and
          (not XmlStateData.FermatInMeasurePart[XmlStateData.MeasureNumber]) then
             inc(XmlStateData.FermataMissing);
          XmlState:=XmlStateMeasureEnd;
          CheckSegnoCodaCapo;
          end
        else if Symbol.StateSymbol=XmlStateDirectionBegin then
          begin
          XmlState:=XmlStateDirectionBegin;
          end
        else if Symbol.StateSymbol=XmlStateAttributesBegin then
          XmlState:=XmlStateAttributesBegin
        else
        // Allow unknown data, just wait for new note
          XmlState:=XmlStateMeasureBegin;
////          XmlStateData.MeasureTime:=XmlStateData.XmlTimeIndex;
////          XmlError('Unknown definition (<'+Symbol.InfoName+
////                   ' '+Symbol.InfoType+'>'+') in "midi-instrument"');
        XmlStateData.Attribute:=AttributeNone;   ////vvvvvvvvvvvv
        XmlStateData.NoteIsGraceNote:=false;
        end;

      XmlStateDurationBegin:
        begin
        // Duration exist only as symbol - states are
        // XmlStateDurationPitchBegin and XmlStateDurationRestBegin
        XmlError('Internal error');
        end;
      XmlStateDurationEnd:
        begin
        XmlError('Internal error');
        end;
      XmlStateMeasureEnd:
        begin
        // Aligned in start or end of measure?


  if XmlStateData.XmlTimeIndex>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName);   ////

        XmlStateData.XmlTimeIndex:=XmlStateData.XmlTimeIndexMaxInMeasure;
        if Symbol.StateSymbol=XmlStatePartEnd then
          begin
          // If repeat begin exist, then there should be an end also
////          XmlStateData.RepeatBeginOpen:=false;


  if XmlStateData.XmlTimeIndex+XmlStateData.Duration>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName); ////


          for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
            PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
          XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;

          // An implicit repeat end may be there
////          if XmlStateData.RepeatInfoCurrent.RepeatBeginOpen and

          if (XmlStateData.RepeatIndex>XmlStateData.RepeatIndexMin) and
            (((XmlStateData.VoltaText='') and (XmlStateData.RepeatInfoCurrent.RepeatNumber=1)) or
               (Pos(IntToStr(XmlStateData.RepeatInfoCurrent.RepeatNumber),XmlStateData.VoltaText)>0))
            then
            begin
            XmlStateData.RepeatInfoCurrent.RepeatInMeasure:=true;
            XmlStateData.RepeatInfoCurrent.RepeatBeginOpen:=false;
            SetFilePosition(DataIn,XmlStateData.RepeatInfoCurrent.RepeatStart,0);
            ReadXmlBufferIndex:=0;
            XmlDataInCount:=0;
            XmlLineNumber:=XmlStateData.RepeatInfoCurrent.RepeatStartLineNumber;
              XmlStateData.NoteNumber:=XmlStateData.RepeatInfoCurrent.RepeatNoteNumber;
            XmlState:=XmlStateData.RepeatInfoCurrent.RepeatStartState;
            inc(XmlStateData.RepeatInfoCurrent.RepeatNumber);
            XmlDelta:=XmlStateData.CommonMultiplicator*XmlStateData.Beats*
              4 div (XmlStateData.BeatType);
              XmlStateData.MeasureTime:=XmlStateData.MeasureTime+XmlDelta;
            XmlStateData.VoltaText:='';
              XmlStateData.VoltaTextRepeat:='';

            XmlStateData.Tempo:=XmlStateData.TempoOrg;
            XmlStateData.Ritardando:=0;

            end
          else
            begin
            // New verse ?
              // Test if all text are already used.
              XmlStateData.Tempo:=XmlStateData.TempoOrg;
              XmlStateData.Ritardando:=0;
              XmlStateData.MoreLyric:=false;
              for i:=0 to XmlStateData.NoteNumber-1 do
                if TextHitCount[i]<MaxHitCount[i] then
                XmlStateData.MoreLyric:=true;
{
              if (XmlStateData.VerseNumber>=XmlStateData.MaxMaxVerse) and
                  XmlStateData.MoreLyric then
                XmlStateData.MaxMaxVerse:=XmlStateData.VerseNumber+1;
}
              if not XmlStateData.MoreLyric then
                XmlStateData.MaxVersePart[XmlStateData.PartNumber]:=XmlStateData.VerseNumber;


              if XmlStateData.PartNumber=0 then
                XmlStateData.MaxVerseNumberAll:=XmlStateData.MaxVerseNumberPart;

              begin
              if ((XmlStateData.VerseNumber<XmlStateData.MaxVerseNumberPart) and XmlStateData.MoreLyric)
                 or
                  (XmlStateData.VerseNumber<XmlStateData.MaxMaxVerse) then

            begin
                inc(XmlStateData.VerseNumber);
              SetFilePosition(DataIn,XmlStateData.VerseStart,0);
              ReadXmlBufferIndex:=0;
              XmlDataInCount:=0;
              XmlLineNumber:=XmlStateData.VerseStartLineNumber;
                  XmlStateData.NoteNumber:=0;
              XmlStateData.VerseStartMeasureNumber:=XmlStateData.MeasureNumber;
            XmlStateData.CurrentVolume:=XmlStateData.Volume[XmlStateData.TrackNumber];
            XmlStateData.VoltaText:='';
                  XmlStateData.VoltaTextRepeat:='';

                XmlStateData.RepeatIndex:=1;
                PushRepeat(XmlStateData.RepeatInfoCurrent);
                XmlStateData.RepeatIndexMin:=XmlStateData.RepeatIndex;
                XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;
                XmlStateData.RepeatInfoFine.RepeatNumber:=0;
                XmlStateData.RepeatInfoSegno.RepeatNumber:=0;
                XmlStateData.RepeatInfoCoda.RepeatNumber:=0;
                XmlStateData.RepeatInfoCoda.RepeatStart:=0;

            end
          else
            begin
                  XmlStateData.MaxVersePart[XmlStateData.PartNumber]:=XmlStateData.VerseNumber;
            XmlState:=XmlStatePartEnd;
                  // Adjust for swing type song. Three must be room for the 50% more
                  // time indexes and beats must be divisible by two to do this
            if XmlStateData.Swing and
                    (((XmlStateData.XmlTimeIndex div 2)*3)<XmlStateData.XmlTimeIndexMax) and
                    (((XmlStateData.Beats) mod 2)=0) then
                begin

                j:=((XmlStateData.XmlTimeIndexMax div
                    (12*XmlStateData.Divisions))*
                    (12*XmlStateData.Divisions)-1);

                for i:=((XmlStateData.XmlTimeIndexMax div
                    (12*XmlStateData.Divisions))*
                    (8*XmlStateData.Divisions)-1)
                    downto 0 do
                  begin
                  // Which part of note (swing part)
                    case (i*2 div XmlStateData.Divisions) mod 8 of
                    0,2,4,6:
                      begin
                      PNoteInfo^[j]:=PNoteInfo^[XmlStateData.XmlTimeIndexMax];
                      dec(j);
                      PNoteInfo^[j]:=PNoteInfo^[i];
                      dec(j);
                      end;
                    1,3,5,7:
                      begin
                      PNoteInfo^[j]:=PNoteInfo^[i];
                      dec(j);
                      end;
                    end;
                  end;
                XmlStateData.XmlTimeIndex:=((XmlStateData.XmlTimeIndex+1) div 2)*3+
                                XmlStateData.Divisions*XmlStateData.Beats;


  if XmlStateData.XmlTimeIndex+XmlStateData.Duration>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName); ////



                XmlStateData.Beats:=XmlStateData.Beats*3 div 2;
                end;
              // Write the preprocessed note info to MidiData (global array)
              NoteInfoWrite(MidiData);
              NoteInfoClear;
              end;
            end;
              end;
          end
        else if Symbol.StateSymbol=XmlStateMeasureBegin then
          begin
          XmlStateData.VoltaTextRepeat:=XmlStateData.VoltaText;
          if Symbol.InfoImplicit<>'"yes"' then
            begin
            XmlStateData.MeasureNumber:=StringToInt(Symbol.InfoNumber)
            end
          else
            XmlStateData.MeasureNumber:=0;
          SetSegnoCodaCapo;

          // If measure number is one, then previous notes shall be expanded
          // in order to get measures aligned with midi
          // In that case the measure need not be full
          XmlStateData.RepeatInfoCurrent.RepeatInMeasure:=false;
          // Create an initial pause to align with measures
          if Symbol.InfoNumber='"1"' then //// MeasureNumber????
            begin
            XmlDelta:=(XmlStateData.CommonMultiplicator*XmlStateData.Beats)-
                (XmlStateData.XmlTimeIndex mod
               (XmlStateData.CommonMultiplicator*XmlStateData.Beats));
            ////    Kan delta blive negativt? (gefdu ad modurmalid mitt)
            if XmlDelta>0 then
              begin
            for i:=XmlStateData.XmlTimeIndex downto 0 do
              begin
                Assert(XmlStateData.XmlTimeIndex+XmlDelta<NumberOfTimeSamples);
              PNoteInfo^[i+XmlDelta]:=PNoteInfo^[i];
              end;
            for i:=0 to XmlDelta-1 do
              begin
              PNoteInfo^[i].NoteOn:=[];
              PNoteInfo^[i].NoteOff:=[];
              PNoteInfo^[i].NoteContinue:=[];
              PNoteInfo^[i].SongText:='';
              end;
              end;
            XmlStateData.XmlTimeIndex:=XmlStateData.XmlTimeIndex+XmlDelta;

  if XmlStateData.XmlTimeIndex+XmlStateData.Duration>=NumberOfTimeSamples then
    XmlError('Too many time divisions (notes) in '+FileName); ////



            XmlStateData.XmlTimeIndexMaxInMeasure:=XmlStateData.XmlTimeIndex;
            end;
          XmlStateData.MeasureTime:=XmlStateData.XmlTimeIndex;
          XmlState:=XmlStateMeasureBegin;
          end
        else
          begin
////          XmlError('Xml Error: Expected new measure or end of part');
        end;
        end;
      XmlStatePartEnd:
        begin
        // If more tracks in one part, set file pointer back and simulate
        // <part ... > XmlStatePartBegin
        if (XmlStateData.TrackNumber<XmlStateData.MidiInstrumentMax-1) and
           (XmlStateData.MidiInstrumentPart[XmlStateData.TrackNumber]=
               XmlStateData.MidiInstrumentPart[XmlStateData.TrackNumber+1]) then
          begin
          // More tracks in same part. Clear the whole note array (NoteInfo)
          NoteInfoClear;
          SetFilePosition(DataIn,XmlStateData.VerseStart,0);
            ReadXmlBufferIndex:=0;
            XmlDataInCount:=0;
          Symbol.StateSymbol:=XmlStatePartBegin;
          Symbol.InfoId:=XmlStateData.PartMap[XmlStateData.MidiInstrumentPart[XmlStateData.TrackNumber+1]];
          XmlLineNumber:=XmlStateData.VerseStartLineNumber;
          XmlStateData.MeasureNumber:=1;
          end;
        XmlStateData.FermataMissing:=0;
        inc(XmlStateData.TrackNumber);

        for i:=XmlStateData.CurrentVolumeStart to XmlStateData.XmlTimeIndex do
           PNoteInfo^[i].Volume:=XmlStateData.CurrentVolume;
        XmlStateData.CurrentVolumeStart:=XmlStateData.XmlTimeIndex;
        // Stop all notes before adding the end-section
        for i:=0 to 127 do
        if XmlStateData.NoteOn[XmlStateData.PartNumber,i] then
          begin
          XmlMidiNoteOff(XmlStateData,i);
          end;
        if Symbol.StateSymbol=XmlStateScorePartWiseEnd then
          begin
          XmlState:=XmlStateEnd;
          end
        else if Symbol.StateSymbol=XmlStatePartBegin then
          begin
          XmlStateData.PartId:=Symbol.InfoId;
          XmlStateData.PartNumber:=LookUpPartId(XmlStateData.PartId);
          if XmlStateData.PartNumber>=0 then
          XmlStateData.CurrentVolume:=XmlStateData.Volume[XmlStateData.TrackNumber];
          NoteInfoGraceIndex:=1;
          NoteInfoGraceIndexStart:=1;
          
          XmlStateData.CurrentVolumeStart:=0;
          XmlStateData.VoltaText:='';
            XmlStateData.VoltaTextRepeat:='';
          XmlStateData.VerseNumber:=1;
          XmlStateData.VerseStart:=GetFilePos(DataIn);
          XmlStateData.VerseStartLineNumber:=XmlLineNumber;
          XmlStateData.VerseStartMeasureNumber:=XmlStateData.MeasureNumber;
          XmlStateData.StaffNumber:=1;
          XmlStateData.RepeatInfoCurrent.RepeatStart:=GetFilePos(DataIn);
          XmlStateData.RepeatInfoCurrent.RepeatBeginOpen:=false;
          XmlStateData.RepeatInfoCurrent.RepeatStartLineNumber:=XmlLineNumber;
          XmlStateData.RepeatInfoCurrent.RepeatStartState:=XmlStateRepeatPartBegin;
          XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;
            XmlStateData.RepeatInfoCurrent.RepeatNoteNumber:=XmlStateData.NoteNumber;
          XmlStateData.RepeatIndex:=1;
          PushRepeat(XmlStateData.RepeatInfoCurrent);
          XmlStateData.RepeatInfoCurrent.RepeatNumber:=1;

          XmlStateData.RepeatInfoFine.RepeatNumber:=0;
          XmlStateData.RepeatInfoSegno.RepeatNumber:=0;
          XmlStateData.RepeatInfoCoda.RepeatNumber:=0;
          XmlStateData.RepeatInfoCoda.RepeatStart:=0;

          XmlStateData.XmlTimeIndex:=0; ////
          XmlStateData.XmlTimeIndexMaxInMeasure:=0;

          XmlStateData.BeforeFirstMeasure:=true;
          XmlStateData.RepeatInfoCurrent.RepeatBeginOpen:=false;
          XmlStateData.Transpose:=0;
          XmlStateData.MeasureTime:=0;
          XmlStateData.MidiInstrumentIndex:=0;
            FillChar(NoteHitCount,sizeof(THitCount),0);
            FillChar(TextHitCount,sizeof(THitCount),0);
            FillChar(MaxHitCount,sizeof(THitCount),0);
          XmlState:=XmlStatePartBegin;
          end
        else
          XmlError('After </part>, expected </score-partwise> or <part>');
          // Fuglin i ....
        end;
      XmlStateEnd:
        begin
        XmlError('Xml Error: Expected new measure or end of part');
        end;
      else
        begin
        end;
      end;
    end;

  if not (XmlState in [XmlStateEnd,XmlStateError]) then
    begin
    XmlError(LinguaTextIncorrectMusicXmlFile+XmlStateTexts[XmlState]+
                LinguaTextExpectedXmlStateEnd);
    FileName:='';
    SystemState:=MidiNoFile;
    Errors:=1;
    end;
  XmlMidiEnd(XmlStateData,MidiData);
  MidiData.MidiDataIndex:=0;
  FileClose(DataIn);

    // Check for number of verses different i different parts
    // If first estimate was wrong then reduce and rerun
      begin
      XmlStateData.MaxMaxVerse:=XmlStateData.MaxVersePart[0];
      for i:=1 to XmlStateData.PartNumberMax-1 do
        begin
        if XmlStateData.MaxMaxVerse<XmlStateData.MaxVersePart[i] then
        XmlStateData.MaxMaxVerse:=XmlStateData.MaxVersePart[i];
        end;
      // Minimum one verse (even if no text)
      if XmlStateData.MaxMaxVerse<1 then XmlStateData.MaxMaxVerse:=1;

      // If maxVerseNumbers are different then recheck
      XmlStateData.MaxVerseCheck:=false;
      if not (XmlState in [XmlStateError]) then
      for i:=0 to XmlStateData.PartNumberMax-1 do
        begin
        XmlStateData.MaxVerseCheck:=XmlStateData.MaxVerseCheck or
              (XmlStateData.MaxMaxVerse<>XmlStateData.MaxVersePart[i]);
        end;
////      XmlStateData.MaxVerseCheck:=XmlStateData.MaxMaxVerse<XmlStateData.MaxVerseNumberPart;
////      XmlStateData.MaxVerseNumberPart:=XmlStateData.MaxMaxVerse;
      end;
    end;
  end;



end.

module Instruments where

data Instrument =
  Trumpet |
  Trombone |
  Piano |
  KettleDrum |
  Violin |
  Viola |
  Cello |
  Oboe |
  Clarinet |
  Bassoon |
  Guitar |
  Flute |
  Sitar

showInstrument :: Instrument -> String
showInstrument Trumpet = "trumpet"
showInstrument Trombone = "trombone"
showInstrument Piano = "piano"
showInstrument KettleDrum = "kettledrum"
showInstrument Violin = "violin"
showInstrument Viola = "viola"
showInstrument Cello = "cello"
showInstrument Oboe = "oboe"
showInstrument Clarinet = "clarinet"
showInstrument Bassoon = "bassoon"
showInstrument Guitar = "guitar"
showInstrument Flute = "flute"
showInstrument Sitar = "sitar"

instance Show Instrument where
  show = showInstrument

data InstrumentType =
  Strings |
  Percussion |
  Woodwinds |
  Brass 

showInstrumentType :: InstrumentType -> String
showInstrumentType Strings = "strings"
showInstrumentType Percussion = "percussion"
showInstrumentType Woodwinds = "woodwinds"
showInstrumentType Brass = "brass"

instance Show InstrumentType where
  show = showInstrumentType

getInstrumentType :: Instrument -> InstrumentType
getInstrumentType Trumpet = Brass
getInstrumentType Trombone = Brass
getInstrumentType Piano = Percussion
getInstrumentType KettleDrum = Percussion
getInstrumentType Violin = Strings
getInstrumentType Viola = Strings
getInstrumentType Cello = Strings
getInstrumentType Oboe = Woodwinds
getInstrumentType Clarinet = Woodwinds
getInstrumentType Bassoon = Woodwinds
getInstrumentType Guitar = Strings
getInstrumentType Flute = Brass
getInstrumentType Sitar = Strings

getInstrumentType2 :: Instrument -> InstrumentType
getInstrumentType2 Trumpet = Brass
getInstrumentType2 Trombone = Brass
getInstrumentType2 Piano = Percussion
getInstrumentType2 KettleDrum = Percussion
getInstrumentType2 Oboe = Woodwinds
getInstrumentType2 Clarinet = Woodwinds
getInstrumentType2 Bassoon = Woodwinds
getInstrumentType2 Flute = Brass
getInstrumentType2 _ = Strings

getInstrumentType3 :: Instrument -> InstrumentType
getInstrumentType3 x =
  case x of
   Trumpet -> Brass
   Trombone -> Brass
   Piano -> Percussion
   KettleDrum -> Percussion
   Violin -> Strings
   Viola -> Strings
   Cello -> Strings
   Oboe -> Woodwinds
   Clarinet -> Woodwinds
   Bassoon -> Woodwinds
   Guitar -> Strings
   Flute -> Brass
   Sitar -> Strings
    

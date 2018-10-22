MidiAndMusicXmlPlayerTest runs MidiAndMusicXmlPlayer in seven instances in parallel. At the end the program WinMerge is called to compare the result to the original file-test-output. Use Delphi XE or later to compile it.

The program takes all files with the extension .xml as argument to MidiAndMusicXmlPlayer (runs in test mode - options "-T"). 
The files are supposed to be in a directoty called "new". Another directory called "org" has the original test results. At the end of the program the program "Windiff" is called to compare the files.

Note: Considaration: The program should be more general. Arguments should select which extension, how many processes to run and what program to analise the result.


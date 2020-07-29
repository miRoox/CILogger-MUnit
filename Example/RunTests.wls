#!/usr/bin/env wolframscript

Needs["MUnit`"]
Get@FileNameJoin@{DirectoryName[$InputFileName, 2], "CILogger", "CILogger.wl"}

Exit[1-Boole@MUnit`TestRun["Tests.wlt", "Loggers" -> {CILogger[]}]]

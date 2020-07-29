#!/usr/bin/env wolframscript

Needs["MUnit`"]
Get@FileNameJoin@{DirectoryName[$InputFileName, 2], "CILogger", "CILogger.wl"}

MUnit`TestRun["Tests.wlt", "Loggers" -> {CILogger[]}]

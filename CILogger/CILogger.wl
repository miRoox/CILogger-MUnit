(* Mathematica Package *)

BeginPackage["CILogger`", {"MUnit`"}];

CILogger::usage =
    "CILogger[] is an MUnit logger for continuous integration.";

Begin["`Private`"];

hasEnvironmentQ[name_] := !FailureQ@Environment[name]

$CIQ = hasEnvironmentQ["CI"];
$GitHubActionQ = $CIQ && hasEnvironmentQ["GITHUB_ACTIONS"];
$TravisCIQ = $CIQ && hasEnvironmentQ["TRAVIS"];
$CircleCIQ = $CIQ && hasEnvironmentQ["CIRCLECI"];

hostCI=Which[
  $GitHubActionQ, "GitHub Actions",
  $TravisCIQ, "Travis CI",
  $CircleCIQ, "CircleCI",
  $CIQ, "Unknown",
  True, "Local"
]

toInputString[HoldForm[expr_]] := ToString[Unevaluated[expr], InputForm]
toInputString[expr_] := ToString[expr, InputForm]

CILogger[] :=
    With[{logger = Unique["MUnit`Loggers`Private`logger"]},
      Module[{detail = <||>},
        logger /: MUnit`LogStart[logger, title_] :=
          (
            WriteString[$Output, "Operating System: " <> $SystemID <> "\n"];
            WriteString[$Output, "Host on: " <> hostCI <> "\n"];
            WriteString[$Output, "Wolfram Language Version: " <> $Version <> "\n"];
            WriteString[$Output, "Starting test run" <> If[title === None, "", ": " <> ToString@title] <> "\n"];
          );
        logger /: MUnit`LogBeginTestSection[logger, section_, (*require*)_] :=
          WriteString[$Output, "Starting test section ", section, "\n"];
        logger /: MUnit`LogEndTestSection[logger] :=
          WriteString[$Output, "\nEnding test section\n"];
        logger /: MUnit`LogMessage[logger, msg_String] :=
          WriteString[$Output, "\n" <> msg <> "\n"];
        logger /: MUnit`LogFatal[logger, msg_String] :=
          WriteString[$Output, "\n" <> msg <> "\n"];
        logger /: MUnit`LogFatal[logger, msg_String] /; $GitHubActionQ :=
          WriteString[$Output, "\n::error::" <> msg <> "\n"];
        logger /: MUnit`LogSuccess[logger, (*tr*)_?MUnit`TestResultQ] :=
          WriteString[$Output, "."];
        logger /: MUnit`LogFailure[logger, tr_?MUnit`TestResultQ] :=
          Module[{msg = MUnit`TestFailureMessage[tr]},
            Which[
              $GitHubActionQ, WriteString[$Output, TemplateApply["::error file=<*$TestFileName*>::"]],
              True, WriteString[$Output, "!\n"]
            ];
            WriteString[$Output, "Test number " <> ToString@MUnit`TestIndex[tr] <> " with TestID " <> ToString@MUnit`TestID[tr] <> " had a failure.\n"];
            WriteString[$Output, "\tInput: " <> toInputString@MUnit`TestInput[tr] <> "\n"];
            WriteString[$Output, "\tExpected output: " <> toInputString@MUnit`ExpectedOutput[tr] <> "\n"];
            WriteString[$Output, "\tActual output: " <> toInputString@MUnit`ActualOutput[tr] <> "\n"];
            WriteString[$Output, "\tExpected messages: " <> toInputString@MUnit`ExpectedMessages[tr] <> "\n"];
            WriteString[$Output, "\tActual messages: " <> toInputString@MUnit`ActualMessages[tr] <> "\n"];
            If[msg =!= "", WriteString[$Output, "\n** " <> ToString[msg] <> " **\n"]]
          ];
        logger /: MUnit`LogMessagesFailure[logger, tr_?MUnit`TestResultQ] :=
          Module[{msg = MUnit`TestFailureMessage[tr]},
            Which[
              $GitHubActionQ, WriteString[$Output, TemplateApply["::warning file=<*$TestFileName*>::"]],
              True, WriteString[$Output, "*\n"]
            ];
            WriteString[$Output, "Test number " <> ToString@MUnit`TestIndex[tr] <> " with TestID " <> ToString@MUnit`TestID[tr] <> " had a messages failure.\n"];
            WriteString[$Output, "\tInput: " <> toInputString@MUnit`TestInput[tr] <> "\n"];
            WriteString[$Output, "\tExpected messages: " <> toInputString@MUnit`ExpectedMessages[tr] <> "\n"];
            WriteString[$Output, "\tActual messages: " <> toInputString@MUnit`ActualMessages[tr] <> "\n"];
            If[msg =!= "", WriteString[$Output, "\n** " <> ToString[msg] <> " **\n"]]
          ];
        logger /: MUnit`LogError[logger, tr_?MUnit`TestResultQ] :=
          Module[{msg = MUnit`ErrorMessage[tr]},
            Which[
              $GitHubActionQ, WriteString[$Output, TemplateApply["::error file=<*$TestFileName*>::"]],
              True, WriteString[$Output, "!\n"]
            ];
            WriteString[$Output, "Test number " <> ToString@MUnit`TestIndex[tr] <> " with TestID " <> ToString@MUnit`TestID[tr] <> " had an error.\n"];
            If[msg =!= "", WriteString[$Output, "\n** " <> msg <> " **\n"]]
          ];
        logger /: MUnit`LogAbsoluteTimeUsed[logger, time_] :=
          (detail[["AbsoluteTime"]] = Quantity[time, "Second"]);
        logger /: MUnit`LogCPUTimeUsed[logger, time_] :=
          (detail[["CPUTime"]] = Quantity[time, "Second"]);
        logger /: MUnit`LogMemoryUsed[logger, mem_] :=
          (detail[["Memory"]] = Quantity[mem, "Byte"]);
        logger /: MUnit`LogEnd[logger, testCnt_, (*successCnt*)_, failCnt_, msgFailCnt_, skippedTestCnt_, errorCnt_, abort_] :=
          (
            If[abort, WriteString[$Output, "\nTest run stopped before completion.\n"]];
            WriteString[$Output, "\nElapsed Time: " <> ToString[detail[["AbsoluteTime"]]]];
            WriteString[$Output, "\nElapsed CPU Time: " <> ToString[detail[["CPUTime"]]]];
            WriteString[$Output, "\nMemory Used: " <> ToString[detail[["Memory"]]]];
            WriteString[$Output, "\nTests run: " <> ToString[testCnt]];
            WriteString[$Output, "\nFailures: " <> ToString[failCnt]];
            WriteString[$Output, "\nMessages Failures: " <> ToString[msgFailCnt]];
            WriteString[$Output, "\nSkipped Tests: " <> ToString[skippedTestCnt]];
            WriteString[$Output, "\nErrors: " <> ToString[errorCnt]];
            WriteString[$Output, "\nFatal: " <> ToString[abort]];
            WriteString[$Output, "\n\n"];
          );
        logger /: Format[logger, StandardForm] :=
          Interpretation[Row[{RawBoxes["\[SkeletonIndicator]"], RawBoxes["CI Logger"], RawBoxes["\[SkeletonIndicator]"]}], logger];
        logger /: Format[logger, OutputForm] :=
          "-CI Logger-";
        logger
      ]
    ]

End[]; (* `Private` *)

EndPackage[]
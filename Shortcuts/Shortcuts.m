(* Wolfram Language Package *)
(* Author:: Rolf Mertig :: *)

(*
This


(* load the MathematicaPackageInstall function from github *)



(* install the Shortcuts` package from github into $UserBaseDirectory/Applications *)
MathematicaPackageInstall`MathematicaPackageInstall["Shortcuts`"];

Needs["Shortcuts`"];                     (* load it and open the example notebook *)

Shortcuts`InstallShortcuts[]

*)

BeginPackage["Shortcuts`"]

InstallShortcuts::usage = "InstallShortcuts[] installs additional shortcuts to $UserBaseDirectory/SystemFiles/FrontEnd/TextResource/... ."

UninstallShortcuts::usage = "UninstallShortcuts[] deletes the file KeyEventTranslations.tr and joker.m in $UserBaseDirecty/SystemFiles/FrontEnd/TextResources/..."



Get @ FileNameJoin[{DirectoryName[$InputFileName], "ShortcutCode.m"}];


(* Implementation of the package *)


(* ::Subtitle:: *)
(*Date: May 19, 2015*)


(* ::Subsubtitle:: *)
(*Author: Rolf Mertig, http://www.gluonvision.com *)


(* ::Text:: *)

(* Note:  InstallShortcuts[] generates

   FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources",
                 Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", "Windows", "Unix", "X"],
                 "KeyEventTranslations.tr" }]
 and

*   FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources",
                 Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", "Windows", "Unix", "X"],
                 "joker.m"}]
*)

(* If you want to get rid of all extra keyboard shortcuts, while retaining the original ones, run:
   UninstallShortcuts[]

*)

UninstallShortcuts[] :=
    If[ FileExistsQ[#],
      DeleteFile[#];
      If[ !FileExistsQ[#],
        Print["Deleted "<>#],
        Print["Could not delete "<>#]
      ]
    ]&/@ Map[FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources",
      Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", "Windows", "Unix", "X"],#}]&,
      {"KeyEventTranslations.tr", "joker.m"}
    ];


(* ::Subsection:: *)
(* Region Title *)
InstallShortcuts[] :=
    If[$VersionNumber < 9,
      Print["This code works only in Wolfram Mathematica 9 and higher."],


      If[True,
        SetOptions[$FrontEnd, NotebookSecurityOptions -> {"TrustByDefault" -> True, "UntrustedPath" -> {}}]
        ,
        CreateWindow[DialogNotebook[{TextCell["
     The extra keyboard shortcuts to be installed call the Kernel and \
therefore can prompt the \"Enable Dynamic\" button. This can be \
circumvented by accepting the following change to your Mathematica \
setup:

     Do you agree to permanently trust all
     notebook evaluations? If you click OK, the followoing will be \
evaluated, potentially asking for another
     decision.
      SetOptions[$FrontEnd, NotebookSecurityOptions ->
     {\"TrustByDefault\" -> True, \"UntrustedPath\" -> {}}]
     ", "Text", Background -> White],
          Row[{Spacer[142],
            DefaultButton[
              DialogReturn[
                SetOptions[$FrontEnd,
                  NotebookSecurityOptions -> {"TrustByDefault" -> True,
                    "UntrustedPath" -> {}}]
              ],
              ImageSize -> {42 GoldenRatio, 42}],
            Spacer[42],
            CancelButton[ImageSize -> {42 GoldenRatio, 42}]}]},
          WindowFloating -> True, WindowTitle -> "Security change",
          Background -> LightBlue]]
      ];


      Block[ {mykeyevents},
        mykeyevents =
            "
(* Test Run 1: delete all output, do not restart the kernel and evaluate all cells above  *)
Item[KeyEvent[\"Tab\", Modifiers -> {Control}],
     KernelExecute[ Needs[\"Shortcuts`\"];
                    Shortcuts`Shortcut[\"TestRun1\"]
     ], MenuEvaluator -> Automatic
],
" <>
                "
(* Test Run 2: delete all output, restart the kernel and evaluate all cells above  *)
Item[KeyEvent[\"Tab\", Modifiers -> {Control, Shift}],
     KernelExecute[ Needs[\"Shortcuts`\"];
                    Shortcuts`Shortcut[\"TestRun2\"]
     ], MenuEvaluator -> Automatic
],
" <>

                "
(* Minimize all windows *)
Item[KeyEvent[\"m\", Modifiers -> {Command,Option}],
     KernelExecute[ Needs[\"Shortcuts`\"];
                    Shortcuts`Shortcut[\"Minimize\"]
     ], MenuEvaluator -> Automatic
],
" <>
                "
(* move cell(s) up *)
Item[KeyEvent[\"u\", Modifiers -> {Command,Option}],
     KernelExecute[ Needs[\"Shortcuts`\"];
                    Shortcuts`Shortcut[\"MoveCellsUp\"]
     ], MenuEvaluator -> Automatic
],
" <>
                "
(* move cell(s) down *)
Item[KeyEvent[\"d\", Modifiers -> {Command,Option}],
     KernelExecute[ Needs[\"Shortcuts`\"];
                    Shortcuts`Shortcut[\"MoveCellsDown\"]
     ], MenuEvaluator -> Automatic
],
" <>
                "
(* from inside a cell select the cell bracket *)
Item[KeyEvent[\"b\", Modifiers -> {Command, Option}],
     KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"SelectCellBracket\"] ], MenuEvaluator -> Automatic
],
" <>
                "
(* Delete output: Ctrl Shift x, is a shortcut for  Delete All Output and for NotebookDelete[Cells[MessagesNotebook[]]] *)
Item[KeyEvent[\"x\", Modifiers -> {" <> If[$OperatingSystem === "MacOSX", "Command" , "Control" ] <> ", Shift}],
     KernelExecute[ Needs[\"Shortcuts`\"];
     				  Shortcuts`Shortcut[\"DeleteOutputAndMessages\"] ], MenuEvaluator -> Automatic
],
" <>
                "
(* Delete all cells but Input and Code: Ctrl Alt x, is a shortcut for  Delete All Output and all non-Input and non-Code cells and
           for NotebookDelete[Cells[MessagesNotebook[]]]
   Suggested by Mooniac
           *)
Item[KeyEvent[\"x\", Modifiers -> {" <> If[$OperatingSystem === "MacOSX", "Command" , "Control" ] <> ", Command}],
     KernelExecute[ Needs[\"Shortcuts`\"];
     				  Shortcuts`Shortcut[\"DeleteAllCellsButInputAndCode\"] ], MenuEvaluator -> Automatic
],

" <>
                "
(* Restart that Mathematica FrontEnd which was started last *)
Item[KeyEvent[\"r\", Modifiers -> {Control}],
     KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"RestartFrontEnd\"] ], MenuEvaluator -> Automatic
],
" <>
                "
(* Quit and restart the Kernel; using this by Kuba: *)
(* http://mathematica.stackexchange.com/questions/82803/quit-the-kernel-and-start-new-session-automatically *)
Item[KeyEvent[\"q\", Modifiers -> "
                <>
                If[$OperatingSystem === "Unix",
                  "{Control, Shift}",
                  "{Control}"
                ] <>
                "],
     KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"RestartKernel\"] ], MenuEvaluator -> Automatic
],
 " <>
                "
(* Select all Input and Code cells upwards from where the mouse is and evaluate those cells. *)
Item[KeyEvent[\"" <> If[$OperatingSystem === "Unix", ",", "PageUp"] <>
                "\", Modifiers -> {" <>
                Switch[$OperatingSystem, "MacOSX", "Command, Shift", "Windows", "Control, Shift", "Unix", "Command, Option" ] <>

                "}],
         KernelExecute[Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"EvalFromTop\"]], MenuEvaluator -> Automatic
 ],

(* Evaluate Notebook *)
" <>
                "Item[KeyEvent[\"h\", Modifiers -> {Control}],\"EvaluateNotebook\"
],

(* Evaluate Notebook *)
" <>
                "Item[KeyEvent[\"`\", Modifiers -> {Control}],\"EvaluateNotebook\"
],
" <>
                "
(* Select all cells upwards from where the mouse is *)
Item[KeyEvent[\"Up\", Modifiers -> {" <>
                If[$OperatingSystem === "MacOSX", "Command" , "Control" ] <>
                ", Shift}],
         KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"SelectToTop\"] ]
         , MenuEvaluator -> Automatic
 ],
" <>
                "
(* Select all cells downwards from where the mouse is *)

Item[KeyEvent[\"Down\", Modifiers -> {" <>
                If[$OperatingSystem === "MacOSX", "Command" , "Control" ] <>
                ", Shift}],
         KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"SelectToBottom\"] ]
         , MenuEvaluator -> Automatic
 ],
" <>
                "
(* Close all Untitled* Notebooks *)

Item[KeyEvent[\"Delete\", Modifiers -> {Control, Shift}],
         KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"CloseUntitledNotebooks\"] ]
         , MenuEvaluator -> Automatic
 ],
" <>
                "
(* by rm-rf: http://mathematica.stackexchange.com/questions/5212/automating-esc-esc-formatting/5215#5215*)
Item[KeyEvent[\"[\"(*]*), Modifiers -> {Control}],
        FrontEndExecute[{
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[],
                \" " <> "\\" <> "[LeftDoubleBracket]\", After]
        }]
],
Item[KeyEvent[(*[*)\"]\", Modifiers -> {Control}],
        FrontEndExecute[{
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[],
                \" " <> "\\" <> "[RightDoubleBracket]\", After]
        }]
],
Item[KeyEvent[(*[*)\"]\", Modifiers -> {Control, Command}],
        FrontEndExecute[{
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[],
               \"" <> "\\" <> "[LeftDoubleBracket]\", After],
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[],
               \"" <> "\\" <> "[RightDoubleBracket]\", Before]
        }]
],
(* for german keyboards, for Yves  *)
Item[KeyEvent[\"F4\"],
        FrontEndExecute[{
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[],
               \"" <> "\\" <> "[LeftDoubleBracket]\", After],
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[],
                \"" <> "\\" <> "[RightDoubleBracket]\", Before]
        }]
],

(* on Linux and  Windows: set WindowMargins -> 42 on the selected notebook *)
Item[KeyEvent[\"F6\"],
     FrontEndExecute[{FrontEnd`SetOptions[FrontEnd`SelectedNotebook[], Rule[WindowMargins, 42]]}]
],

(* the 'live' configurable joker keyboard shortcut: *)
Item[KeyEvent[\"" <> If[$OperatingSystem === "MacOSX", "d", "t"] <> "\", Modifiers -> {Control}],
        KernelExecute[{
          Function[
          If[ FileExistsQ[ Slot[1] ],
              Get[ Slot[1] ]
          ]][ FileNameJoin[{$UserBaseDirectory, \"SystemFiles\", \"FrontEnd\", \"TextResources\",
                            Switch[$OperatingSystem, \"MacOSX\", \"Macintosh\", \"Windows\", \"Windows\", \"Unix\", \"X\"],
                            \"joker.m\"}] ]
        }],
  MenuEvaluator -> Automatic
],

(* edit 'live' configurable joker keyboard shortcut: *)
Item[KeyEvent[\"j\", Modifiers -> {Control, Shift}],
        KernelExecute[{
          Function[
          If[ FileExistsQ[ Slot[1] ],
              NotebookOpen[ Slot[1] ]
          ]][ FileNameJoin[{$UserBaseDirectory, \"SystemFiles\", \"FrontEnd\", \"TextResources\",
                            Switch[$OperatingSystem, \"MacOSX\", \"Macintosh\", \"Windows\", \"Windows\", \"Unix\", \"X\"],
                            \"joker.m\"}] ]
        }],
  MenuEvaluator -> Automatic
],

" <>
                "
(* inspired by Kuba: http://mathematica.stackexchange.com/questions/32340/evaluating-selected-expression-using-keyboard-shortcut/32341#32341*)
 Item[KeyEvent[\"" <> Switch[$OperatingSystem, "Windows", ",", "MacOSX", "PageDown", "Unix", "c"] <>
                "\", Modifiers -> {" <>
                Switch[$OperatingSystem, "MacOSX", "Command", "Windows", "Control, Shift", "Unix", "Command, Option"] <>
                "}
      ],
      KernelExecute[
          CreateDocument[
             Function[
             If[ Or[ MatchQ[Slot[1], _String],  MatchQ[ Slot[1], _RowBox] ],
             {
             Cell[ BoxData[Slot[1]],\"Input\"],
             ExpressionCell[ToExpression[Slot[1]],\"Output\"]
             },
             Cell[TextData[\"Please select an expression inside a cell.\"], \"Text\"]
             ]
             ][Replace[NotebookRead[FrontEnd`SelectedNotebook[]  ], {} -> Null]],
             WindowSize->{Medium, FitAll}, WindowMargins -> {{Automatic,2},{Automatic,2}}, Magnification->1.5
     ]
    ], MenuEvaluator -> Automatic
 ],

" ;

        With[ {os = Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", "Windows", "Unix", "X"]},
          Quiet@CreateDirectory@FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", os}];
          Module[{
            mykeyeventtrans = FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", os, "KeyEventTranslations.tr"}],
            myjokerfilename = FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", os, "joker.m"}], keytext, mykeytext,
            myjokerdir
          },
            myjokerdir = DirectoryName[myjokerfilename];
            If[ FileExistsQ[mykeyeventtrans], DeleteFile @ mykeyeventtrans];
            If[ FileExistsQ[myjokerfilename], Quiet @ DeleteFile @ myjokerfilename];

            Export[myjokerfilename,
              "
(* Define your own shortcut definitions here, without restarting the FrontEnd.
   The action defined here is executed by Ctrl T (Windows / Linux)  or Ctrl D (MacOSX)
*)

(* uncomment an example below or add your own definitions, save this file without renaming it and use it directly *)

(* Example 1: copy the selection as LaTeX. Evaluating sel pastes the clipboard *)

(*
  FrontEnd`CopyToClipboard[FrontEnd`CopyAsTeX[]];
  (* and also function assignments: *)
  sel := Paste[];
*)

(* Example 2:  wrap selection in [ ] *)

(*
  NotebookApply[SelectedNotebook[], RowBox[{\"[\", \"\\[SelectionPlaceholder]\", \"]\"}], Before]
*)


",
              "Text", CharacterEncoding :> $CharacterEncoding
            ];

            (* start with the default one : *)
            CopyFile[FileNameJoin[{$InstallationDirectory, "SystemFiles", "FrontEnd", "TextResources",
              os, "KeyEventTranslations.tr"}], mykeyeventtrans];

            keytext = Import[mykeyeventtrans, "Text"];
            (* and add our extra keyevents: *)
            mykeytext = StringReplace[keytext, "EventTranslations[{"(*]*) :> StringJoin["EventTranslations[{\n "(*]*), mykeyevents, "\n "]];
            (* make sure things work in Mathematica 9 and 10 :*)

            mykeytext =
                Composition[# <> "\n" &,
                  Function[StringReplace[#, "Item[KeyEvent[\"" ~~ it_ ~~ "\", Modifiers" ~~ ( mo__ /; StringLength[mo] < 30) ~~ "\"Redo\"]" (*]*) :>
                      "If[$VersionNumber < 10, {}, Item[KeyEvent[" ~~ it ~~ ", Modifiers" ~~ mo ~~ " \"Redo\"]]" (*]*), 1
                  ]
                  ],
                  Function[
                    StringReplace[#, "Item[KeyEvent[\"Redo\"]"(*]*) -> "If[$VersionNumber < 10, {}, Item[KeyEvent[\"Redo\"]]" (*]*), 1 ]
                  ]
                ] /@ ImportString[mykeytext, "Lines", CharacterEncoding :> $CharacterEncoding];
            Export[mykeyeventtrans, mykeytext, "Text", CharacterEncoding :> $CharacterEncoding];

            CellPrint[ExpressionCell[Column[{ "Two files have been generated:",
              ( Button[StringReplace[#, $UserBaseDirectory -> "$UserBaseDirectory"],
                CreateDocument[{ TextCell[ Import[#, "Text"]]}]
              ]&@ mykeyeventtrans
              ),
              "and",
              ( Button[StringReplace[#, $UserBaseDirectory -> "$UserBaseDirectory"], NotebookOpen[#]]& @ myjokerfilename)}], "Text", ShowStringCharacters -> False]];
            CellPrint[TextCell["After restarting the FrontEnd once the following extra keyboard shortcuts can be used: ", "Text", FontWeight -> Bold]];
            CellPrint[Cell["?Shortcuts","Input"]];
            thisnb = EvaluationNotebook[];
            CellPrint[ExpressionCell[Information["Shortcuts", LongForm -> True],"Output"]];
            With[{nb = thisnb},
            CreateWindow@DialogNotebook[{
            	           TextCell["Restart the FrontEnd now? All Untitled notebooks will be closed automatically.", FontSize -> 20],
                            DefaultButton[DialogReturn[Shortcut["RestartFrontEnd", nb]], ImageSize -> {50 GoldenRatio, 50}],
                            CancelButton[ImageSize->{50 GoldenRatio, 50}] 
                            }]
            ]
            	(*
            With[{myjokerdir2 = StringReplace[myjokerdir, $UserBaseDirectory -> "$UserBaseDirectory"]},
              Switch[ $OperatingSystem,
                "Windows",
                CellPrint[TextCell[#, "Text", FontSize -> 14, ShowStringCharacters -> False]&@
                    Grid[{

                      { "Ctrl Shift \[UpArrow] ", "Select all cells from the beginning of the notebook until the cursor position "},
                      { "Ctrl Shift \[DownArrow] ", "Select all cells from the position of the mouse until the end of the notebook "},
                      { "Ctrl Shift PageUp ", "Evaluate all cells from the beginning of the notebook until the position of the mouse "},
                      { "Ctrl Shift X ", "Delete all generated cells, like Output, Message and Print cells, also in MessagesNotebook[] "},
                      { "Ctrl Q ", "Quit and restart Kernel "},
                      { "Ctrl R", "Quit and restart the FrontEnd; all Untitled notebooks are closed and others saved. Save and reopen the selected notebook if possible."},
                      { "Ctrl ` ", "Evaluate Notebook"},
                      { "Ctrl H ", "Evaluate Notebook"},
                      { "Ctrl Tab ", "Delete all output and evaluate all cells from the beginning of the ntoebook until the cursor position "},
                      { "Ctrl Shift Tab ", "Delete all output, restart the kernel and evaluate all cells from the beginning of the ntoebook until the cursor position "},
                      { "Ctrl Shift ,", "Copy/Evaluate/Paste the selected cell subexpression into a new notebook "},
                      { "Ctrl Shift X ", "Delete all generated cells, like Output, Message and Print cells "},
                      { "Cmd Alt B ", "Move the cursor from anywhere inside a cell to the cell bracket "},
                      { "Cmd Alt M ", "Minimize all Mathematica notebooks "},
                      { "Cmd Alt U ", "Cut the selected cell and paste it before the preceding cell "},
                      { "Cmd Alt D ", "Cut the selected cell and paste it after the following cell "},
                      { "Ctrl Shift Delete ", "Close all Untitled-*  notebooks without confirmation "},
                      { "F4 ", "Insert [[]]"},
                      { "F6 ", "Apply SetOptions[SelectedNotebook[], WindowMargins -> 42]"},
                      { "Ctrl T ", "Evaluate the file joker.m from " <> myjokerdir2},
                      { "Ctrl Shift J ", "Open joker.m from " <> myjokerdir2},
                      { Framed[
                        Style["The following three shortcuts work only on english keyboards: ", "Text"],
                        FrameStyle -> None, FrameMargins -> 10],
                        SpanFromLeft
                      },
                      { "Ctrl [ "(*]*), "Insert [["},
                      { (*[*)"Ctrl ] ", "Insert ]]"},
                      { (*[*)"Ctrl Alt ] ", "Insert [[]]"}
                    }, Alignment -> {Left}, Dividers -> All, FrameStyle -> LightGray]
                ],

                "MacOSX",
                CellPrint[TextCell[#, "Text", FontSize -> 14, ShowStringCharacters -> False]&@
                    Grid[{
                      { "Cmd Shift \[UpArrow] ", "Select all cells from the beginning of the notebook until the position of the mouse "},
                      { "Cmd Shift \[DownArrow]  ", "Select all cells from the position of the mouse until the end of the notebook "},
                      { "Fn Shift Cmd \[UpArrow] ", "Evaluate all cells from the beginning of the notebook until the position of the mouse "},
                      { "Cmd Shift X ", "Delete all generated cells, like Output, Message and Print cells "},
                      { "Ctrl R ", "Quit and restart the FrontEnd"},
                      { "Ctrl Q ", "Quit and restart Kernel"},
                      { "Ctrl H ", "Evaluate Notebook  "},
                      { "Ctrl ` ", "Evaluate Notebook  "},
                      { "Fn Cmd \[DownArrow] ", "Copy/Evaluate/Paste the selected Input cell subexpression into a new notebook "},
                      { "Cmd Opt M ", "Minimize all Mathematica notebooks "},
                      { "Fn Shift Ctrl Delete ", "Delete all Untitled-*  notebooks without confirmation "},
                      { "Ctrl D ", "Evaluate the file joker.m in " <> myjokerdir2},
                      { "Ctrl Shift J ", "Open joker.m from " <> myjokerdir2},
                      { Framed[
                        Style["The following three shortcuts work only on for english keyboards: ", "Text"],
                        FrameStyle -> None, FrameMargins -> 10], SpanFromLeft
                      },
                      { "Ctrl [ "(*]*), "Insert [["},
                      { (*[*)"Ctrl ] ", "Insert ]]"},
                      { (*[*)"Ctrl Cmd ] ", "Insert [[]]"}
                    }, Alignment -> {Left}, Dividers -> All, FrameStyle -> LightGray]
                ]
                ,
                "Unix",
                CellPrint[TextCell["Mod1 and Mod2 are usually the Windows Key and the Alt Key ", ShowStringCharacters -> False]];

                CellPrint[TextCell[#, "Text", FontSize -> 14, ShowStringCharacters -> False]&@
                    Grid[{
                      { "Ctrl Shift \[UpArrow] ", "Select all cells from the beginning of the notebook until the position of the mouse "},
                      { "Ctrl Shift \[DownArrow] ", "Select all cells from the position of the mouse until the end of the notebook "},
                      { "Mod1 Mod2 , ", "Evaluate all cells from the beginning of the notebook until the position of the mouse "},
                      { "Ctrl Shift X ", "Delete all generated cells, like Output, Message and Print cells "},
                      { "Mod1 Mod2 Delete ", "Quit Kernel immediately"},
                      { "Ctrl H ", "Evaluate Notebook "},
                      { "Mod1 Mod2 C ", "Copy/Evaluate/Paste the selected Input cell subexpression into a new notebook "},
                      { "Mod1 Mod2 M ", "Minimize all Mathematica notebooks "},
                      { "Ctrl Shift Delete ", "Delete all Untitled-*  notebooks without confirmation "},
                      { "F4 ", "Insert [[]]"},
                      { "F6 ", "Apply SetOptions[SelectedNotebook[], WindowMargins -> 42]"},
                      { "Ctrl T ", "Evaluate the file joker.m in " <> myjokerdir2},
                      { "Ctrl Shift J ", "Open joker.m from " <> myjokerdir2},
                      { Framed[ Style["The following three shortcuts work only on for english keyboards: ", "Text"], FrameStyle -> None, FrameMargins -> 10], SpanFromLeft },
                      { "Ctrl [ "(*]*), "Insert [["},
                      { (*[*)"Ctrl ] ", "Insert ]]"},
                      { (*[*)"Ctrl Mod2 ] ", "Insert [[]]"}
                    }, Alignment -> {Left}, Dividers -> All, FrameStyle -> LightGray
                    ]]
              ]
            ]
              *)
          ]
        ]
      ]
    ];






EndPackage[]


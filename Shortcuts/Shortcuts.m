(* Author:: Rolf Mertig ; GluonVision GmbH, Berlin, Germany:: *)

(*

(* load the MathematicaPackageInstall function from github *)

(* install the Shortcuts` package from github into $UserBaseDirectory/Applications *)


(
URLSave[
	"https://raw.githubusercontent.com/rolfmertig/Shortcuts/master/Shortcuts.zip",
     FileNameJoin[{$TemporaryDirectory, "Shortcuts.zip"}]
];
ExtractArchive[FileNameJoin[{$TemporaryDirectory, "Shortcuts.zip"}], 
               FileNameJoin[{$UserBaseDirectory, "Applications"}] // Quiet;
];
Needs["Shortcuts`"]; Shortcuts`InstallShortcuts[];
)

*)



BeginPackage["Shortcuts`"]

(* This is just to let the ref guides be displayed without warning *)
If[ 9 <= $VersionNumber < 10,
    SetOptions[$FrontEndSession, MessageOptions -> {"InsufficientVersionWarning" -> False}]
];


Shortcuts::usage = "Shortcuts[] gives a list of extra keyboard shortcuts which were defined by InstallShortcuts[]."

InstallShortcuts::usage = "InstallShortcuts[] adds additional shortcuts to a copy of the system 
FileNameJoin[{$InstallationDirectory, \"SystemFiles\", \"FrontEnd\", \"TextResources\", 
Switch[$OperatingSystem, \"MacOSX\", \"Macintosh\", \"Windows\", \"Windows\", \"Unix\", \"X\"], \"KeyEventTranslations.tr\"}]
to the file
FileNameJoin[{$UserBaseDirectory, \"SystemFiles\", \"FrontEnd\", \"TextResources\", 
Switch[$OperatingSystem, \"MacOSX\", \"Macintosh\", \"Windows\", \"Windows\", \"Unix\", \"X\"], \"KeyEventTranslations.tr\"}]
";

UninstallShortcuts::usage = "UninstallShortcuts[] deletes the file KeyEventTranslations.tr and joker.m in $UserBaseDirecty/SystemFiles/FrontEnd/TextResources/..."


Get @ FileNameJoin[{DirectoryName[$InputFileName], "Shortcut.m"}];

Begin["`Private`"];


(* becuase Shortcuts[] is operating system dependent we need DynamicBox in the help notebooks ... *)
Shortcuts[] :=
    Block[ {toKeyStyle, keyHelp},
        DynamicModule[ {keyStyle},
        Dynamic[
        Style[
                Column[{       
                                  Switch[$OperatingSystem,
                                           "Windows", 
                                           "In the following table the " <>
                                           keyStyle["Win"] <> 
                                             " key is meant to be the Windows key next to the "<>
                                           keyStyle["Alt"] <> " key.\n",
                                           "Unix", 
                                           "In the following table the " <>
                                           keyStyle["Mod1"] <> 
                                             " key is usually the Windows key and the "<>
                                           keyStyle["Mod2"] <> 
                                           " key is usually the " <> keyStyle["Alt"] <> " key",
                                            _ , 
                                          ""
                                   ] 
                                   ,
                Switch[
                   $OperatingSystem,
                   
                   "Windows",                   
                   
                (* Windows *)    
                StringReplace[#, "Cmd" -> "Win"]& @ ToString[#, StandardForm]& @ Grid[ toKeyStyle /@ 
                    ( 
                {#, keyHelp[#]}& /@ {
                (*keyevent[26]*) "Ctrl F1",
                (*keyevent[1] *) "Ctrl Tab",
                (*keyevent[2] *) "Ctrl Shift Tab",
                (*keyevent[12]*) "Ctrl Shift \[UpArrow]",
                (*keyevent[15]*) "Ctrl ;", 
                (*keyevent[16]*) "Ctrl Shift \[DownArrow]",
                (*keyevent[7] *) "Ctrl Shift X", 
                (*keyevent[8] *) "Ctrl Alt X", 
                (*keyevent[11]*) "Ctrl Q", 
                (*keyevent[9] *) "Ctrl R", 
                (*keyevent[13]*) "Ctrl H", 
                (*keyevent[25]*) "Ctrl Shift ,", 
                (*keyevent[6] *) "Cmd Alt B", 
                (*keyevent[3] *) "Cmd Alt M",
                (*keyevent[4] *) "Cmd Alt U",
                (*keyevent[5] *) "Cmd Alt D",
                (*keyevent[10]*) "Cmd Alt Delete",
                (*keyevent[17]*) "Ctrl Shift Delete",
                (*keyevent[21]*) "F4",
                (*keyevent[12]*) "F6",
                (*keyevent[12]*) "Ctrl T",
                (*keyevent[12]*) "Ctrl Shift J", 
                (*keyevent[28]*) "Ctrl F2", 
                (*keyevent[12]*) Framed[ Style["The following shortcuts do only work on english keyboard layouts:", "Text"], 
                                         Background -> LightGray, FrameStyle -> None, FrameMargins -> 10], 
                (*keyevent[14]*) "Ctrl `", 
                (*keyevent[27]*) "Ctrl Shift /", 
                (*keyevent[12]*) "Ctrl ["(*]*), 
                (*[*)
                                 "Ctrl ]", 
                (*keyevent[12]*) (*[*)"Ctrl Alt ]"
                }),
                gridOptions
                ]
                    ,
                (* ******************************** *********************************************************** *)     
                (* Linux *)
                "Unix"
                ,
                StringReplace[#, {"Cmd" -> "Mod1", "Alt" -> "Mod2"}]& @ ToString[#, StandardForm]& @ Grid[ toKeyStyle /@ 
                    ( {#, keyHelp[#]}& /@ {
                      (*keyevent[26]*) "Ctrl F1",
                (*keyevent[1] *) "Ctrl Tab",
                (*keyevent[2] *) "Ctrl Shift Tab",
                (*keyevent[12]*) "Ctrl Shift \[UpArrow]",
                (*keyevent[15]*) "Ctrl ;", 
                (*keyevent[16]*) "Ctrl Shift \[DownArrow]",
              (*keyevent[7] *) "Ctrl Alt X", 
              (*keyevent[8] *) "Cmd Alt X", 
                (*keyevent[11]*) "Ctrl Shift Q", 
                (*keyevent[9] *) "Ctrl R", 
                (*keyevent[13]*) "Ctrl H", 
                (*keyevent[25]*) "Cmd Alt C", 
                (*keyevent[6] *) "Cmd Alt B", 
                (*keyevent[3] *) "Cmd Alt M",
                (*keyevent[4] *) "Cmd Alt U",
                (*keyevent[5] *) "Cmd Alt D",
                (*keyevent[10]*) "Cmd Alt Delete",
                (*keyevent[17]*) "Ctrl Shift Delete",
                (*keyevent[21]*) "F4",
                (*keyevent[12]*) "F6",
                (*keyevent[12]*) "Ctrl T",
                (*keyevent[12]*) "Ctrl Shift J", 
                (*keyevent[28]*) "Ctrl F2", 
                (*keyevent[27]*) "Cmd Alt I", 
                (*keyevent[12]*) Framed[ Style["The following shortcuts do only work on english keyboard layouts:", "Text"], 
                                         Background -> LightGray, FrameStyle -> None, FrameMargins -> 10], 
                (*keyevent[14]*) "Ctrl `", 
                (*keyevent[12]*) "Ctrl ["(*]*), 
                                         (*[*)
                                 "Ctrl ]", 
                (*keyevent[12]*) (*[*)"Ctrl Alt ]"
                }),
                gridOptions               
                ]
                ,
                "MacOSX"
                ,
                StringReplace[#, {"Cmd" -> "\[CloverLeaf]", "Alt" -> "Alt"}]& @ ToString[#, StandardForm]& @ Grid[ toKeyStyle /@
                  ( {#, keyHelp[#]}& /@ {
               (*keyevent[26]*) "Cmd Escape",
                (*keyevent[1] *) "Ctrl Tab",
                (*keyevent[2] *) "Ctrl Shift Tab",
                (*keyevent[12]*)   "Cmd Shift \[UpArrow]",
                (*keyevent[16]*) "Cmd Shift \[DownArrow]",
                (*keyevent[7] *) "Cmd Shift X", 
                (*keyevent[11]*) "Ctrl Q", 
                (*keyevent[9] *) "Ctrl R", 
                (*keyevent[13]*) "Ctrl H", 
                (*keyevent[25]*) "Cmd PageDown",
                (*keyevent[15]*) "Cmd PageUp",
                (*keyevent[28]*) "Cmd Home",
                (*keyevent[28]*) "Cmd End",
                (*keyevent[27]*) "Cmd Alt I",
                (*keyevent[6] *) "Cmd Alt B", 
                (*keyevent[3] *) "Cmd Alt M",
                (*keyevent[4] *) "Cmd Alt U",
                (*keyevent[5] *) "Cmd Alt J",
               
                (*keyevent[17]*) "Ctrl Shift Delete",
                (*keyevent[12]*) "Ctrl D",
                (*keyevent[12]*) "Ctrl Shift J", 
                (*keyevent[12]*) Framed[ Style["The following shortcuts do only work on english keyboard layouts:", "Text"], FrameStyle -> None, FrameMargins -> 10], 
                (*keyevent[14]*) "Ctrl `", 
                (*keyevent[12]*) "Ctrl ["(*]*), 
                                         (*[*)
                                 "Ctrl ]"
                } ),
                gridOptions
                ]
                ]}]
        ,"Text"]
        ] (* Dynamic *)
        ,
        Initialization :> (
            gridOptions =  {Alignment -> {Left, Center}, Dividers -> All, FrameStyle -> LightGray, Spacings -> {2,1}};
            jokerdir = StringReplace[ FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", 
                        Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", "Windows", "Unix", "X"] }], $UserBaseDirectory -> "$UserBaseDirectory"];
            keyHelp[z_ /; Head[z] =!= String] :=
                SpanFromLeft;
            keyHelp["Ctrl F1"]                 =  (* Windows, Linux *)
            keyHelp["Cmd Escape"]               = "Open the Shortcuts documentation page listing all extra keyboard shortcuts.";  (* MacOSX *)
            keyHelp["Ctrl Tab"]                = "Delete all output and evaluate all cells from the top to the insertion point.";
            keyHelp["Ctrl Shift Tab"]          = "Delete all output, restart the kernel and evaluate all cells from the top to the insertion point.";
            keyHelp["Ctrl Shift \[UpArrow]"]   = "Evaluate all cells from the top of the notebook until the insertion point.";
            keyHelp["Ctrl ;"]                  =  (* Windows, Linux *)
            keyHelp["Cmd Home"]                = "Select all cells from the top of the notebook to the insertion point.";
            keyHelp["Ctrl Shift \[DownArrow]"] = "Select all cells from the insertion point until the end of the notebook.";
            keyHelp["Ctrl Shift Tab"]          = "Delete all output, restart the kernel and evaluate all cells from the top to the insertion point.";
            keyHelp["Ctrl Shift \[UpArrow]"]   =  (* Windows, Linux *)
            (* Macintosh *)
            keyHelp["Cmd Shift \[UpArrow]"]    =  "Evaluate all cells from the top of the notebook until the insertion point.";
            keyHelp["Ctrl ;"]                  = "Select all cells from the top of the notebook to the insertion point.";
            keyHelp["Ctrl Shift \[DownArrow]"] =  (* Windows, Linux *)
            keyHelp["Cmd Shift \[DownArrow]"]  = "Select all cells from the insertion point until the end of the notebook.";
            keyHelp["Ctrl Shift X"]            =  (* Windows *)
            keyHelp["Ctrl Cmd X"]              =  (* Linux *)
            keyHelp["Cmd Shift X"]             =  (* Macintosh *) "Delete all generated cells, like Output, Message and Print cells, also in MessagesNotebook[].";
            keyHelp["Ctrl Alt X"]              =  (* Windows *)
            keyHelp["Cmd Alt X"]               = "Delete all non-Input and non-Code cells."; (* Linux *) (*  does not work on Macintosh *)
            keyHelp["Ctrl Q"]                  =  (* Windows *)
            keyHelp["Ctrl Shift Q"]            = "Quit and restart the kernel."; (* Linux *)
            keyHelp["Ctrl R"]                  = "Quit and restart the front end. All Untitled notebooks are closed without confirmation." <> 
                                                  " All others are saved." <>
                                                  " The selected notebook is saved and reopened, if possible.";
            keyHelp["Ctrl H"]                  = "Evaluate Notebook.";
            keyHelp["Ctrl Shift ,"]            =  (* Windows*)
            keyHelp["Cmd Alt C"]               =  (* Linux *)
            keyHelp["Cmd PageDown"]            = "Copy, paste and evaluate the selected expression from the inside of a cell into a new notebook."; (*MacOSX*)
            keyHelp["Cmd Alt B"]               = "Select the current cell. The insertion point can be anywhere inside the cell.";
            keyHelp["Cmd Alt M"]               = "Minimize all notebooks.";
            keyHelp["Cmd Alt U"]               = "Cut the selected cell and paste it before the preceding cell.";
            keyHelp["Cmd Alt D"]               = (* Windows, Linux *)
            keyHelp["Cmd Alt J"]               = "Cut the selected cell and paste it after the following cell.";
            keyHelp["Cmd Alt Delete"]          = "Quit the front end, saving named notebooks first.";
            keyHelp["Ctrl Shift Delete"]       = "Close all Untitled notebooks without confirmation.";
            keyHelp["F4"]                      = "Insert \[LeftDoubleBracket]\[RightDoubleBracket]";
            keyHelp["F6"]                      =  (* Windows, Linux *)
            keyHelp["Cmd PageUp"]              = "Stack windows.";
            keyHelp["Ctrl T"]                  =  (* Windows, Linux *)
            keyHelp["Ctrl D"]                  = "Evaluate the user defined code written in joker.m from " <> jokerdir;
            keyHelp["Ctrl Shift J"]            = "Open the user configurable file joker.m from " <> jokerdir;
            keyHelp["Ctrl F2"]                 =  (* Windows, Linux *)
            keyHelp["Cmd End"]                 = "Open $UserBaseDirectory by SystemOpen[$UserBaseDirectory].";
            keyHelp["Ctrl `"]                  = "Evaluate Notebook.";
            keyHelp["Ctrl Shift /"]            =  (* Windows *)
            keyHelp["Cmd Alt I"]               =  (* MacOSX, Linux *) "Open the init.m file found by FindFile[\"init.m\"] in the front end."; (* Linux *)
            keyHelp["Ctrl ["]  (*]*)           = "Insert [["; (* ]] *)
            (* [[[ *)
            keyHelp["Ctrl ]"]                  = "Insert ]]"; 
            (*[*)
            keyHelp["Ctrl Alt ]"]              = "Insert [[]]";
            keyStyle[s_] :=
                ToString[ Framed[ Style[s, FontFamily -> "Courier", FontColor -> GrayLevel[0.365], 
                    FontWeight -> "Bold"],
                ImageMargins -> {{2, 2}, {2, 2}}, FrameStyle -> GrayLevel[0.8], Background -> GrayLevel[0.965] ], StandardForm
                ];
            toKeyStyle[{keyshort_String, desc_String}] :=
                ( 
                      {Row @ Riffle[ keyStyle /@ StringSplit[keyshort] , " + "], desc } 
                );
            toKeyStyle[{a_, b_Symbol}] :=
                {a, b};
               )
        ]
    (* DynamicModule *)
            ];



(* Implementation of the package *)


(* ::Subtitle:: *)
(*Date: May 19, 2015*)


(* ::Subsubtitle:: *)
(*Author: Rolf Mertig, GluonVision GmbH, http://www.gluonvision.com *)


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

(* InstallShortcuts[] asks if NotebookSecurityOptions should be changed and 
   installs
  *)

InstallShortcuts[] :=
    If[ $VersionNumber < 9,
        Print["This code works only in Wolfram Mathematica 9 and higher."],
        If[ False, (* change to True just for debugging *)
            SetOptions[$FrontEnd, "NotebookSecurityOptions" -> {"TrustByDefault" -> True, "UntrustedPath" -> {}}],
            DialogInput[{TextCell[
            "The extra keyboard shortcuts to be installed call the kernel and \
therefore can prompt the \"Enable Dynamic\" button. This can be \
avoided by accepting the following change to your Mathematica \
setup, which you should only accept if you understand the consequences: 

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
                          "NotebookSecurityOptions" -> {"TrustByDefault" -> True,
                            "UntrustedPath" -> {}}]
                      ],
                  ImageSize -> {42 GoldenRatio, 42}],
                Spacer[42],
                CancelButton[ImageSize -> {42 GoldenRatio, 42}],
                Spacer[10]
                }
                ]},
              WindowFloating -> True, WindowTitle -> "Security change",
              Background -> LightBlue]
        ];
        Block[ {mykeyevents , (* keyevent, *)keyHelp},
            keyevent["TestRun1"] = keyevent[1] = 
            "
(* Test Run 1: delete all output, do not restart the kernel and evaluate all cells above  *)
	Item[	KeyEvent[\"Tab\", Modifiers -> {Control}], KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"TestRun1\"] ], MenuEvaluator -> Automatic
	], 
";
            keyevent["TestRun2"] = keyevent[2] = 
            "
(* Test Run 2: delete all output, restart the kernel and evaluate all cells above  *)
	Item[	KeyEvent[\"Tab\", Modifiers -> {Control, Shift}],
			KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"TestRun2\"] ], MenuEvaluator -> Automatic
    ],
";
            keyevent["Minimize"] = keyevent[3] = 
            "
(* Minimize all windows *)
	Item[	KeyEvent[\"m\", Modifiers -> {Command, Option}],
			KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"Minimize\"] ], MenuEvaluator -> Automatic
	],
";
            keyevent["MoveCellsUp"] = keyevent[4] = 
            "
(* move cell(s) up *)
	Item[	KeyEvent[\"u\", Modifiers -> {Command, Option}],
			KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"MoveCellsUp\"] ], MenuEvaluator -> Automatic
	],
";
            keyevent["MoveCellsDown"] = keyevent[5] = 
            "
(* move cell(s) down *)
	Item[	KeyEvent[\"" <> If[ $OperatingSystem === "MacOSX",
                                 "j",
                                 "d"
                             ] <> "\", Modifiers -> {Command, Option}],
			KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"MoveCellsDown\"] ], MenuEvaluator -> Automatic
	],
";
            keyevent["SelectCellBracket"] = keyevent[6] = 
            "
(* from inside a cell select the cell bracket *)
	Item[	KeyEvent[\"b\", Modifiers -> {Command, Option}],
			KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"SelectCellBracket\"] ], MenuEvaluator -> Automatic
	],
";
            keyevent["DeleteOutputAndMessages"] = keyevent[7] = 
            "
(* Delete output: Ctrl Shift x, is a shortcut for  Delete All Output and for NotebookDelete[Cells[MessagesNotebook[]]] *)
	Item[	KeyEvent[\"x\", Modifiers -> " <> 
                                        Switch[$OperatingSystem, "MacOSX",  "{Command, Shift}", 
                                                                 "Windows", "{Control, Shift}",
                                                                 "Unix",    "{Control, Command}"
                                        ] <> 
                                              "],
			KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"DeleteOutputAndMessages\"] ], MenuEvaluator -> Automatic
	],
";
            keyevent["DeleteAllCellsButInputAndCode"] = keyevent[8] =
            If[ $OperatingSystem === "MacOSX",
                ""(* does not work on MacOSX *)
               ,
                "
(* Delete all cells but Input and Code: Ctrl Alt x, is a shortcut to delete all Output and all 
           non-Input and non-Code cells and does also NotebookDelete[Cells[MessagesNotebook[]]]
           Suggested by Mooniac
*)
	Item[	KeyEvent[\"x\", Modifiers -> {" <> Switch[ $OperatingSystem,
                                                           "Windows",
                                                            "Control, Command",
                                                            "Unix",
                                                            "Command, Option"
                                                ] <> "}],
			KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"DeleteAllCellsButInputAndCode\"] ], MenuEvaluator -> Automatic
	],
"
            ];
            keyevent["RestartFrontEnd"] = keyevent[9] = 
            "
(* Restart that Mathematica front end which was started last *)
	Item[	KeyEvent[\"r\", Modifiers -> {Control}],
				KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"RestartFrontEnd\"] ], MenuEvaluator -> Automatic
	],
";
            keyevent["QuitFrontEnd"] = keyevent[10] = 
            If[ $OperatingSystem === "MacOSX",
                "",
                "
(* Quit the last Mathematica front end *)
	Item[KeyEvent[\"Delete\", Modifiers -> {Command, Option}],
            KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"QuitFrontEnd\"] ], MenuEvaluator -> Automatic
	],
"
            ];
            keyevent["RestartKernel"] = keyevent[11] = 
            "
(* Quit and restart the kernel; using this by Kuba: *)
			(* http://mathematica.stackexchange.com/questions/82803/quit-the-kernel-and-start-new-session-automatically *)
	Item[KeyEvent[\"q\",  Modifiers -> " <> 
                                      If[ $OperatingSystem === "Unix",
                                          "{Control, Shift}",
                                          "{Control}"
                                      ] <>
                    "],     
           KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"RestartKernel\"] ], MenuEvaluator -> Automatic
	],
";
            keyevent["EvaluateFromTop"] = keyevent[12] = 
            "
(* Select all Input and Code cells upwards from where the mouse is and evaluate those cells. *)
	Item[KeyEvent[\"Up\", Modifiers -> {" <>
                                            If[ $OperatingSystem === "MacOSX",
                                                "Command",
                                                "Control"
                                            ] <> ", Shift}
         ],
         KernelExecute[Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"EvaluateFromTop\"]], MenuEvaluator -> Automatic
	],";
            keyevent["EvaluateNotebook"] = keyevent[13] = 
            "
(* Evaluate Notebook and move to the end *)
	Item[KeyEvent[\"h\", Modifiers -> {Control}
		 ], 
         KernelExecute[Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"EvaluateNotebook\"]], MenuEvaluator -> Automatic
	],
";
            keyevent["EvaluateNotebook2"] = keyevent[14] = 
            "
(* Evaluate Notebook *)
	Item[KeyEvent[\"`\", Modifiers -> {Control}, \"EvaluateNotebook\"
		 ], 
         KernelExecute[Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"EvaluateNotebook\"]], MenuEvaluator -> Automatic
	],
";
            keyevent["SelectToTop"] = keyevent[15] = 
            "
(* Select all cells upwards from where the mouse is *)
	Item[KeyEvent[\"" <> If[ $OperatingSystem === "MacOSX",
                             "Home",
                             ";"
                         ] <> "\", Modifiers -> {" <>
                                            If[ $OperatingSystem === "MacOSX",
                                                "Command",
                                                "Control"
                                            ] <> "}
         ],
         KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"SelectToTop\"] ] , MenuEvaluator -> Automatic
	],";
            keyevent["SelectToBottom"] = keyevent[16] = 
                            "
(* Select all cells downwards from where the mouse is *)
	Item[KeyEvent[\"Down\", Modifiers -> {" <>
                                            If[ $OperatingSystem === "MacOSX",
                                                "Command",
                                                "Control"
                                            ] <>
                                        ", Shift}],
         KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"SelectToBottom\"] ]
         , MenuEvaluator -> Automatic
	],";
            keyevent["CloseUntitledNotebooks"] = keyevent[17] = 
                "
	(* Close all Untitled* Notebooks *)
	Item[KeyEvent[\"Delete\", Modifiers -> {Control, Shift}],
         KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"CloseUntitledNotebooks\"] ]
         , MenuEvaluator -> Automatic
	],";
            keyevent["[["] = keyevent[18] = 
                "
	(* by rm-rf: http://mathematica.stackexchange.com/questions/5212/automating-esc-esc-formatting/5215#5215*)
	Item[KeyEvent[\"[\"(*]*), Modifiers -> {Control}],
        FrontEndExecute[
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[], \" " <> "\\" <> "[LeftDoubleBracket]\", After]
        ]
	],";
            keyevent["]]"] = keyevent[19] = 
                "
	Item[KeyEvent[(*[*)\"]\", Modifiers -> {Control}],
        FrontEndExecute[
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[], \" " <> "\\" <> "[RightDoubleBracket]\", After] 
        ]
	],";
            keyevent["[[]]"] = keyevent[20] = 
                "
	Item[KeyEvent[(*[*)\"]\", Modifiers -> {Control, Command}],
        FrontEndExecute[{
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[],
               \"" <> "\\" <> "[LeftDoubleBracket]\", After],
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[],
               \"" <> "\\" <> "[RightDoubleBracket]\", Before]
        }]
	],";
            keyevent["F4"] = keyevent[21] = 
                "
	(* for german keyboards, for Yves  *)
	Item[KeyEvent[\"F4\"],
        FrontEndExecute[{
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[], \"" <> "\\" <> "[LeftDoubleBracket]\", After],
            FrontEnd`NotebookWrite[FrontEnd`InputNotebook[], \"" <> "\\" <> "[RightDoubleBracket]\", Before]
        }]
	],";
            keyevent["F6"] = keyevent[22] = 
                "
	(* on Linux and  Windows: Stack windows  by F6, on MacOSX by Cmd PageUp*)
	Item[KeyEvent[\""<> If[ $OperatingSystem === "MacOSX",
                            "PageUp",
                            "F6"
                        ] <> "\"" <>
            If[ $OperatingSystem === "MacOSX",
                ", Modifiers -> {Command}",
                ""
            ] <> "],
	    " <> "KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"F6\"] ] , MenuEvaluator -> Automatic
	],";
            keyevent["RunJoker"] = keyevent[23] = 
                "
	(* run 'live' configurable joker keyboard shortcut: *)
	Item[KeyEvent[\"" <> If[ $OperatingSystem === "MacOSX",
                             "d",
                             "t"
                         ] <> "\", Modifiers -> {Control}],
        KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"RunJoker\"] ], MenuEvaluator -> Automatic 
	],";
            keyevent["OpenJoker"] = keyevent[24] = 
                "
	(* edit 'live' configurable joker keyboard shortcut: *)
	Item[KeyEvent[\"j\", Modifiers -> {Control, Shift}],
        KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"OpenJoker\"] ], MenuEvaluator -> Automatic
	],";
            keyevent["EvaluateSelected"] = keyevent[25] = 
                "
	(* evaluate selected expression in a new notebook *)               
	(* inspired by Kuba: http://mathematica.stackexchange.com/questions/32340/evaluating-selected-expression-using-keyboard-shortcut/32341#32341 *)
	Item[KeyEvent[\"" <> Switch[
                                          $OperatingSystem, 
                                          "Windows", ",", 
                                          "MacOSX", "PageDown", 
                                          "Unix", "c"
                                       ] <> "\", Modifiers -> {" <> 
                                       Switch[$OperatingSystem, 
                                               "MacOSX", "Command", 
                                               "Windows", "Control, Shift", 
                                               "Unix", "Command, Option"
                                       ] <>
                            "}
      ],
      KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"EvaluateSelected\"] ], MenuEvaluator -> Automatic 
      ],";
            keyevent["OpenShortcutsHelp"] = keyevent[26] =
            If[ $OperatingSystem === "MacOSX",
                "
  Item[KeyEvent[\"Escape\", Modifiers -> {Command}],
        KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"OpenShortcutHelpPage\"]], MenuEvaluator -> Automatic
	],",
                "
  Item[KeyEvent[\"F1\", Modifiers -> {Control}],
        KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"OpenShortcutHelpPage\"]], MenuEvaluator -> Automatic
	],"
            ];
            keyevent["OpenInitFile"] = keyevent[27] = 
                "
	(* open the init.m file *)
	Item[KeyEvent[\"" <> Switch[$OperatingSystem, "Unix", "I",
                                                  "Windows", "/",
                                                  "MacOSX", "I"
                         ]<>"\", Modifiers -> {" <> Switch[$OperatingSystem,
                                                   "Windows",
                                                   "Control, Shift",
                                                   "Unix",
                                                   "Command, Option",
                                                   "MacOSX",
                                                   "Command, Option"
                                            ] <> " }],
        KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"OpenInit.m\"]], MenuEvaluator -> Automatic
	],";
                
                
                (* *)
            keyevent["OpenUserBaseDirectory"] = keyevent[28] = 
                "
	(* open the $UserBaseDirectory file *)
	Item[KeyEvent[\""<>If[ $OperatingSystem === "MacOSX",
                           "End",
                           "F2"
                       ]<>"\", Modifiers -> {" <> If[ $OperatingSystem === "MacOSX",
                                                      "Command",
                                                      "Control"
                                                  ] <>"}],
        KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"OpenUserBaseDirectory\"]], MenuEvaluator -> Automatic
	],";
            mykeyevents = StringJoin @@ Array[keyevent, 28];

            (* *************************************************************************************************** *)
            (* myjokerfilename: a keyboard shortcut configurable shortcut *)
            With[ {os = Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", "Windows", "Unix", "X"]},
                Quiet@CreateDirectory@FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", os}];
                Module[ {
                  mykeyeventtrans = FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", os, "KeyEventTranslations.tr"}],
                  myjokerfilename = FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", os, "joker.m"}], 
                  keytext, mykeytext,
                  myjokerdir, helpopen
                },
                    myjokerdir = DirectoryName[myjokerfilename];
                    If[ FileExistsQ[mykeyeventtrans],
                        DeleteFile @ mykeyeventtrans
                    ];
                    If[ FileExistsQ[myjokerfilename],
                        Quiet @ DeleteFile @ myjokerfilename
                    ];
                    Export[myjokerfilename,
                      "
(* Define your own shortcut definitions here, without restarting the front end.
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
                    
         (* ********************************************************************************************************* *)           

        (* extend the default KeyEventTranslations.tr from $InstallationDirectory : *)
                    CopyFile[FileNameJoin[{$InstallationDirectory, "SystemFiles", "FrontEnd", "TextResources", os, "KeyEventTranslations.tr"}
                             ], 
                             mykeyeventtrans
                    ];
                    keytext = Import[mykeyeventtrans, "Text"];
                    (* and add our extra keyevents: *)
                    mykeytext = StringReplace[keytext, "EventTranslations[{"(*]*) :> StringJoin["EventTranslations[{\n "(*]*), mykeyevents, "\n "]];
                    
                    (* make sure things work in Mathematica 9 and 10 :*)
                    mykeytext = StringJoin[
                        Composition[# <> "\n" &,
                          Function[StringReplace[#, "Item[KeyEvent[\"" ~~ it_ ~~ "\", Modifiers" ~~ ( mo__ /; StringLength[mo] < 30) ~~ "\"Redo\"]" (*]*) :>
                              "If[$VersionNumber < 10, {}, Item[KeyEvent[" ~~ it ~~ ", Modifiers" ~~ mo ~~ " \"Redo\"]]" (*]*), 1
                          ]
                          ],
                          Function[
                            StringReplace[#, "Item[KeyEvent[\"Redo\"]"(*]*) -> "If[$VersionNumber < 10, {}, Item[KeyEvent[\"Redo\"]]" (*]*), 1 ]
                          ]
                        ] /@ ImportString[mykeytext, "Lines", CharacterEncoding :> $CharacterEncoding]
                    ];
                    If[ StringQ[mykeytext],
                        Export[mykeyeventtrans, mykeytext, "Text", CharacterEncoding :> $CharacterEncoding],
                        Print["OH NO!, MT is not a string. Exiting.", Quit[]];
                    ];
                    helpopen :=SetSelectedNotebook @ NotebookOpen[
                                            FileNameJoin[{
                                                 $UserBaseDirectory, "Applications", "Shortcuts", "Documentation", "English", "ReferencePages", "Symbols",
                                                 "Shortcuts.nb"}],
                                                  WindowSize -> {Scaled[1/2], If[ $OperatingSystem==="Unix",
                                                                                  Scaled[.9],
                                                                                  Scaled[1]
                                                                              ]}, 
                                                 WindowMargins -> 
                                                     If[ $OperatingSystem === "Unix",
                                                         {{8, Automatic}, {Automatic, 8}},
                                                         {{0, Automatic}, {Automatic, 0}}
                                                     ]
                                      ];
                     helpopen;                      
                     If[$OperatingSystem === "Unix",
                     	FrontEndTokenExecute@"StackWindows";
                     	helpopen
                     ];
                    (*
                    CellPrint[ExpressionCell[Column[{ "Two files have been generated:",
                      ( Button[StringReplace[#, $UserBaseDirectory -> "$UserBaseDirectory"],
                        CreateDocument[{ TextCell[ Import[#, "Text"]]}]
                      ]&@ mykeyeventtrans
                      ),
                      "and",
                      ( Button[StringReplace[#, $UserBaseDirectory -> "$UserBaseDirectory"], NotebookOpen[#]]& @ myjokerfilename)}], "Text", ShowStringCharacters -> False]
                      ];
                      *)
                    With[ {nb = SelectedNotebook[]},
                        DialogInput[{
                                       TextCell["Restart the front end now? All Untitled notebooks will be closed automatically.", FontSize -> 20],
                                       Row[{DefaultButton[DialogReturn[Shortcut["RestartFrontEnd", nb]], ImageSize -> {50 GoldenRatio, 50}],
                                            CancelButton[ImageSize->{50 GoldenRatio, 50}] }]
                                    }]
                    ]
                ]
            ]
        ]
    ];

End[];
EndPackage[]
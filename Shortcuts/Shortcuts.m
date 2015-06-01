(* Wolfram Language Package *)
(* Author:: Rolf Mertig :: *)

(*
This


(* load the MathematicaPackageInstall function from github *)


(* install the Shortcuts` package from github into $UserBaseDirectory/Applications *)
MathematicaPackageInstall`MathematicaPackageInstall["Shortcuts`"];

Needs["Shortcuts`"];                     

*)

BeginPackage["Shortcuts`"]

Shortcuts::usage = "Shortcuts[] gives a list of extra keyboard shortcuts which were defined by InstallShortcuts[]."

InstallShortcuts::usage = "InstallShortcuts[] installs additional shortcuts to $UserBaseDirectory/SystemFiles/FrontEnd/TextResource/... ."

UninstallShortcuts::usage = "UninstallShortcuts[] deletes the file KeyEventTranslations.tr and joker.m in $UserBaseDirecty/SystemFiles/FrontEnd/TextResources/..."


Get @ FileNameJoin[{DirectoryName[$InputFileName], "ShortcutCode.m"}];

Begin["`Private`"];


(* becuase Shortcuts[] is operating system dependent we need DynamicBox in the help notebooks ... *)
Shortcuts[] := DynamicModule[ {keyStyle, toKeyStyle},
Dynamic[
Style[
  With[{os = Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", "Windows", "Unix", "X"]},
   With[{ myjokerdir2 = StringReplace[ FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", 
          "TextResources", os}], $UserBaseDirectory -> "$UserBaseDirectory"]
        },
 Column[{       
	"In the following table the " <>
                   Switch[$OperatingSystem,
							"Windows", 
							keyStyle["Win"] <> 
                     	     " key is meant to be the Windows key next to the "<>
						    keyStyle["Alt"] <> "key.\n",
							"Unix", 
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
 StringReplace[#, "Cmd" -> "Win"]& @ ToString[#, StandardForm]& @ Grid[ toKeyStyle /@ {
(*keyevent[26]*) {"Ctrl F1", "Open the Shortcuts tutorial with a list of all extra keyboard shortcuts."}, 
(*keyevent[1] *) {"Ctrl Tab", "Delete all output and evaluate all cells from the top to the insertion point."}, 
(*keyevent[2] *) {"Ctrl Shift Tab", "Delete all output, restart the kernel and evaluate all cells from the top to the insertion point."}, 
(*keyevent[12]*) {"Ctrl Shift \[UpArrow]", "Evaluate all cells from the beginning of the notebook until the insertion point."}, 
(*keyevent[15]*) {"Ctrl ;",   "Select all cells from the beginning of the notebook until the insertion point."}, 
(*keyevent[16]*) {"Ctrl Shift \[DownArrow]", "Select all cells from the insertion point until the end of the notebook."}, 
(*keyevent[7] *) {"Ctrl Shift X", "Delete all generated cells, like Output, Message and Print cells, also in MessagesNotebook[]."}, 
(*keyevent[8] *) {"Ctrl Alt X", "Delete all non-Input and non-Code cells."}, 
(*keyevent[11]*) {"Ctrl Q", "Quit and restart the Kernel."}, 
(*keyevent[9] *) {"Ctrl R", "Quit and restart the FrontEnd; all Untitled notebooks are closed and others saved. Reopening the selected notebook if it is saved."}, 
(*keyevent[13]*) {"Ctrl H", "Evaluate Notebook."}, 
(*keyevent[25]*) {"Ctrl Shift ,", "Copy, paste and evaluate the selected expression from the inside of a cell into a new notebook."}, 
(*keyevent[6] *) {"Cmd Alt B", "Select current cell. The insertion point can be anywhere inside the cell."}, 
(*keyevent[3] *) {"Cmd Alt M", "Minimize all Mathematica notebooks."}, 
(*keyevent[4] *) {"Cmd Alt U", "Cut the selected cell and paste it before the preceding cell."}, 
(*keyevent[5] *) {"Cmd Alt D", "Cut the selected cell and paste it after the following cell."}, 
(*keyevent[10]*) {"Cmd Alt Delete", "Quit the FrontEnd, saving named notebooks first."},
(*keyevent[17]*) {"Ctrl Shift Delete", "Close all Untitled-*  notebooks without confirmation."}, 
(*keyevent[21]*) {"F4", "Insert \[LeftDoubleBracket]\[RightDoubleBracket]"}, 
(*keyevent[12]*) {"F6", "Stack Windows."}, 
(*keyevent[12]*) {"Ctrl T", "Evaluate the file joker.m from " <> myjokerdir2}, 
(*keyevent[12]*) {"Ctrl Shift J", "Open joker.m from " <> myjokerdir2}, 
(*keyevent[12]*) {Framed[ Style["The following four shortcuts work only on english keyboard layouts:", "Text"], FrameStyle -> None, FrameMargins -> 10], SpanFromLeft}, 
(*keyevent[14]*) {"Ctrl `", "Evaluate Notebook."}, 
(*keyevent[12]*) {"Ctrl ["(*]*), "Insert [["}, {(*[*)"Ctrl ] ", "Insert ]]"}, 
(*keyevent[12]*) {(*[*)"Ctrl Alt ]", "Insert [[]]"}}, 
Alignment -> {Left}, Dividers -> All, FrameStyle -> LightGray
]
     ,
(* ******************************** *********************************************************** *)     
(* Linux *)
"Unix"
,
 StringReplace[#, {"Cmd" -> "Mod1", "Alt" -> "Mod2"}]& @ ToString[#, StandardForm]& @ Grid[ toKeyStyle /@ {
(*keyevent[26]*) {"Ctrl F1", "Open the Shortcuts tutorial with a list of all extra keyboard shortcuts."}, 
(*keyevent[1] *) {"Ctrl Tab", "Delete all output and evaluate all cells from the top to the insertion point."}, 
(*keyevent[2] *) {"Ctrl Shift Tab", "Delete all output, restart the kernel and evaluate all cells from the top to the insertion point."}, 
(*keyevent[12]*) {"Ctrl Shift \[UpArrow]", "Evaluate all cells from the beginning of the notebook until the insertion point."}, 
(*keyevent[15]*) {"Ctrl ;",   "Select all cells from the beginning of the notebook until the insertion point."}, 
(*keyevent[16]*) {"Ctrl Shift \[DownArrow]", "Select all cells from the insertion point until the end of the notebook."}, 
(*keyevent[7] *) {"Ctrl Shift X", "Delete all generated cells, like Output, Message and Print cells, also in MessagesNotebook[]."}, 
(*keyevent[8] *) {"Ctrl Alt X", "Delete all non-Input and non-Code cells."}, 
(*keyevent[11]*) {"Ctrl Shift Q", "Quit and restart the Kernel."}, 
(*keyevent[9] *) {"Ctrl R", "Quit and restart the FrontEnd; all Untitled notebooks are closed and others saved. Reopening the selected notebook if it is saved."}, 
(*keyevent[13]*) {"Ctrl H", "Evaluate Notebook."}, 
(*keyevent[25]*) {"Cmd Alt C", "Copy, paste and evaluate the selected expression from the inside of a cell into a new notebook."}, 
(*keyevent[6] *) {"Cmd Alt B", "Select current cell. The insertion point can be anywhere inside the cell."}, 
(*keyevent[3] *) {"Cmd Alt M", "Minimize all Mathematica notebooks."}, 
(*keyevent[4] *) {"Cmd Alt U", "Cut the selected cell and paste it before the preceding cell."}, 
(*keyevent[5] *) {"Cmd Alt D", "Cut the selected cell and paste it after the following cell."}, 
(*keyevent[10]*) {"Cmd Alt Delete", "Quit the FrontEnd, saving named notebooks first."},
(*keyevent[17]*) {"Ctrl Shift Delete", "Close all Untitled-*  notebooks without confirmation."}, 
(*keyevent[21]*) {"F4", "Insert \[LeftDoubleBracket]\[RightDoubleBracket]"}, 
(*keyevent[12]*) {"F6", "Stack Windows."}, 
(*keyevent[12]*) {"Ctrl T", "Evaluate the file joker.m from " <> myjokerdir2}, 
(*keyevent[12]*) {"Ctrl Shift J", "Open joker.m from " <> myjokerdir2}, 
(*keyevent[12]*) {Framed[ Style["The following four shortcuts work only on english keyboard layouts:", "Text"], FrameStyle -> None, FrameMargins -> 10], SpanFromLeft}, 
(*keyevent[14]*) {"Ctrl `", "Evaluate Notebook."}, 
(*keyevent[12]*) {"Ctrl ["(*]*), "Insert [["}, {(*[*)"Ctrl ] ", "Insert ]]"}, 
(*keyevent[12]*) {(*[*)"Ctrl Alt ]", "Insert [[]]"}}, 
Alignment -> {Left}, Dividers -> All, FrameStyle -> LightGray
]
,
"MacOSX"
,
 StringReplace[#, {"Cmd" -> "Mod1", "Alt" -> "Opt"}]& @ ToString[#, StandardForm]& @ Grid[ toKeyStyle /@ {
(*keyevent[26]*) {"Ctrl F1", "Open the Shortcuts tutorial with a list of all extra keyboard shortcuts."}, 
(*keyevent[1] *) {"Ctrl Tab", "Delete all output and evaluate all cells from the top to the insertion point."}, 
(*keyevent[2] *) {"Ctrl Shift Tab", "Delete all output, restart the kernel and evaluate all cells from the top to the insertion point."}, 
(*keyevent[12]*) {"Ctrl Shift \[UpArrow]", "Evaluate all cells from the beginning of the notebook until the insertion point."}, 
(*keyevent[15]*) {"Ctrl ;",   "Select all cells from the beginning of the notebook until the insertion point."}, 
(*keyevent[16]*) {"Ctrl Shift \[DownArrow]", "Select all cells from the insertion point until the end of the notebook."}, 
(*keyevent[7] *) {"Ctrl Shift X", "Delete all generated cells, like Output, Message and Print cells, also in MessagesNotebook[]."}, 
(*keyevent[8] *) {"Ctrl Alt X", "Delete all non-Input and non-Code cells."}, 
(*keyevent[11]*) {"Ctrl Q", "Quit and restart the Kernel."}, 
(*keyevent[9] *) {"Ctrl R", "Quit and restart the FrontEnd; all Untitled notebooks are closed and others saved. Reopening the selected notebook if it is saved."}, 
(*keyevent[13]*) {"Ctrl H", "Evaluate Notebook."}, 
(*keyevent[25]*) {"Ctrl Shift ,", "Copy, paste and evaluate the selected expression from the inside of a cell into a new notebook."}, 
(*keyevent[6] *) {"Cmd Alt B", "Select current cell. The insertion point can be anywhere inside the cell."}, 
(*keyevent[3] *) {"Cmd Alt M", "Minimize all Mathematica notebooks."}, 
(*keyevent[4] *) {"Cmd Alt U", "Cut the selected cell and paste it before the preceding cell."}, 
(*keyevent[5] *) {"Cmd Alt D", "Cut the selected cell and paste it after the following cell."}, 
(*keyevent[10]*) {"Cmd Alt Delete", "Quit the FrontEnd, saving named notebooks first."},
(*keyevent[17]*) {"Ctrl Shift Delete", "Close all Untitled-*  notebooks without confirmation."}, 
(*keyevent[21]*) {"F4", "Insert \[LeftDoubleBracket]\[RightDoubleBracket]"}, 
(*keyevent[12]*) {"F6", "Stack Windows."}, 
(*keyevent[12]*) {"Ctrl T", "Evaluate the file joker.m from " <> myjokerdir2}, 
(*keyevent[12]*) {"Ctrl Shift J", "Open joker.m from " <> myjokerdir2}, 
(*keyevent[12]*) {Framed[ Style["The following four shortcuts work only on english keyboard layouts:", "Text"], FrameStyle -> None, FrameMargins -> 10], SpanFromLeft}, 
(*keyevent[14]*) {"Ctrl `", "Evaluate Notebook."}, 
(*keyevent[12]*) {"Ctrl ["(*]*), "Insert [["}, {(*[*)"Ctrl ] ", "Insert ]]"}, 
(*keyevent[12]*) {(*[*)"Ctrl Alt ]", "Insert [[]]"}}, 
Alignment -> {Left}, Dividers -> All, FrameStyle -> LightGray
]

]}]
] (* With *)
] (* With *)
,"Text"]
] (* Dynamic *)
,
Initialization :> (keyStyle[s_] :=
                       ToString[ Framed[ Style[s, FontFamily -> "Verdana", FontColor -> GrayLevel[0.36]], 
                       ImageMargins -> {{2, 2}, {2, 2}}, FrameStyle -> GrayLevel[0.8], Background -> GrayLevel[0.965] ], StandardForm
                       ];
                   toKeyStyle[s_] :=
                       ( s /. {keyshort_String, desc_String} :> {Row @
                       Riffle[ keyStyle /@ StringSplit[keyshort] , "+"],
                       desc } );
                      )
] (* DynamicModule *);



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
(* Region Title *)
InstallShortcuts[] :=
    If[$VersionNumber < 9,
      Print["This code works only in Wolfram Mathematica 9 and higher."],


      If[False, (* change to True just for debugging *)
        SetOptions[$FrontEnd, "NotebookSecurityOptions" -> {"TrustByDefault" -> True, "UntrustedPath" -> {}}]
        ,
        DialogInput[{TextCell[
"The extra keyboard shortcuts to be installed call the Kernel and \
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


Block[ {mykeyevents, keyevent},
	
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
" ;

keyevent["MoveCellsDown"] = keyevent[5] = 
"
(* move cell(s) down *)
	Item[	KeyEvent[\"d\", Modifiers -> {Command,Option}],
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

keyevent["DeleteOutputAndMEssages"] = keyevent[7] = 
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
"
(* Delete all cells but Input and Code: Ctrl Alt x, is a shortcut for  Delete All Output and all non-Input and non-Code cells and
   for NotebookDelete[Cells[MessagesNotebook[]]]
   Suggested by Mooniac
*)
	Item[	KeyEvent[\"x\", Modifiers -> {" <> If[$OperatingSystem === "MacOSX", "Command" , "Control" ] <> ", Command}],
			KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"DeleteAllCellsButInputAndCode\"] ], MenuEvaluator -> Automatic
	],
";


keyevent["RestartFrontEnd"] = keyevent[9] = 
"
(* Restart that Mathematica FrontEnd which was started last *)
Item[KeyEvent[\"r\", Modifiers -> {Control}],
     KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"RestartFrontEnd\"] ], MenuEvaluator -> Automatic
],
" ;

keyevent["QuitFrontEnd"] = keyevent[10] = 
	If[$OperatingSystem === "MacOSX",  
	   "",
       "
       (* Quit the last Mathematica FrontEnd *)
       Item[KeyEvent[\"Delete\", Modifiers -> {Command, Option}],
            KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"QuitFrontEnd\"] ], MenuEvaluator -> Automatic
       ],"
	];


keyevent["RestartKernel"] = keyevent[11] = 
	"(* Quit and restart the Kernel; using this by Kuba: *)
	 (* http://mathematica.stackexchange.com/questions/82803/quit-the-kernel-and-start-new-session-automatically *)
	 Item[KeyEvent[\"q\",  Modifiers -> " <> 
						  If[ $OperatingSystem === "Unix",
                              "{Control, Shift}",
                              "{Control}"
                          ] <>
        "],     
           KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"RestartKernel\"] ], MenuEvaluator -> Automatic
     ],";

keyevent["EvaluateFromTop"] = keyevent[12] = 
	"
	(* Select all Input and Code cells upwards from where the mouse is and evaluate those cells. *)
	Item[KeyEvent[\"Up\", Modifiers -> {" <>
					If[$OperatingSystem === "MacOSX", "Command" , "Control" ] <> ", Shift}
         ],
         KernelExecute[Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"EvaluateFromTop\"]], MenuEvaluator -> Automatic
     ],";

keyevent["EvaluateNotebook"] = keyevent[13] = 
	"
	(* Evaluate Notebook *)
	Item[KeyEvent[\"h\", Modifiers -> {Control}],\"EvaluateNotebook\"
	],";

keyevent["EvaluateNotebook2"] = keyevent[14] = 
    "
	(* Evaluate Notebook *)
    Item[KeyEvent[\"`\", Modifiers -> {Control}],\"EvaluateNotebook\"
	],";

keyevent["SelectToTop"] = keyevent[15] = 
	"
	(* Select all cells upwards from where the mouse is *)
	Item[KeyEvent[\";\", Modifiers -> {" <>
					If[$OperatingSystem === "MacOSX", "Command" , "Control" ] <> "}
         ],
         KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"SelectToTop\"] ] , MenuEvaluator -> Automatic
	],";
	
keyevent["SelectToBottom"] = keyevent[16] = 
	"
	(* Select all cells downwards from where the mouse is *)
	Item[KeyEvent[\"Down\", Modifiers -> {" <>
					If[$OperatingSystem === "MacOSX", "Command" , "Control" ] <>
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
	(* on Linux and  Windows: set WindowMargins -> 42 on the selected notebook *)
	Item[KeyEvent[\"F6\"],
		KernelExecute[ Needs[\"Shortcuts`\"]; Shortcuts`Shortcut[\"F6\"] ] , MenuEvaluator -> Automatic
	],";

keyevent["RunJoker"] = keyevent[23] = 
	"
	(* run 'live' configurable joker keyboard shortcut: *)
	Item[KeyEvent[\"" <> If[$OperatingSystem === "MacOSX", "d", "t"] <> "\", Modifiers -> {Control}],
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
      
      
keyevent["ShowShortcutsTutorial"] = keyevent[26] = 
	"
	(* open the guid page of shortcuts *)
	stem[KeyEvent[\"F1\", Modifiers -> {Control}],
        KernelExecute[ Needs[\"Shortcuts`\"]; NotebookOpen@\"paclet:Shortcuts/tutorial/ShortcutsTutorialLinearSystems\" ], MenuEvaluator -> Automatic
	],";


mykeyevents = StringJoin @@ Array[keyevent, 26];
(* *************************************************************************************************** *)
(* myjokerfilename: a keyboard shortcut configurable shortcut *)
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
            CellPrint[ExpressionCell[Information["Shortcuts", LongForm -> True],"Output"]];
            With[{nb = EvaluationNotebook[]},
            DialogInput[{
            	           TextCell["Restart the FrontEnd now? All Untitled notebooks will be closed automatically.", FontSize -> 20],
                           Row[{DefaultButton[DialogReturn[Shortcut["RestartFrontEnd", nb]], ImageSize -> {50 GoldenRatio, 50}],
                                CancelButton[ImageSize->{50 GoldenRatio, 50}] }]
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

End[];
EndPackage[]
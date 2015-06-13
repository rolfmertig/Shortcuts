(* Wolfram Language package *)
Shortcut::usage = "Shortcut[name] contains the kernel definition for the shortcut name. For a list of shortcuts type ?Shortcuts";


Begin["`Private`"]

Shortcut["SelectCellBracket"] :=
	( "Info: from anywhere inside a cell select the cell brack; like continuous Ctrl .";
	  SelectionMove[SelectedNotebook[], All, Cell]
	);


Shortcut["Minimize"] :=
    ( "Info: this minimizes all windows ";
      FrontEndExecute[ {FrontEndToken[#1, "WindowMiniaturize"]}]& /@ Notebooks[] 
    );

Shortcut["EvaluateNotebook"] :=
    ( "Info: Evaluate Notebook and move to the end";
      FrontEndExecute[ {FrontEndToken[SelectedNotebook[], "EvaluateNotebook"]}];
      FrontEndExecute[{
			FrontEnd`SelectionMove[FrontEnd`InputNotebook[], After, Notebook],
			FrontEnd`FrontEndToken[FrontEnd`InputNotebook[], "ScrollNotebookEnd"]
		}]
    );
    
    
Shortcut["DeleteOutputAndMessages"] :=
    ( "Info: Delete all output (including Print cells) in the selected notebook and contents in the Messages notebook ";
      If[$VersionNumber >= 10, FrontEndTokenExecute @ "DeleteGeneratedCells"];
      NotebookDelete @ Cells[ CellStyle -> ( "Output" | "Print" | "Message" )];
      NotebookDelete @ Cells @ MessagesNotebook[];
    );


Shortcut["DeleteAllCellsButInputAndCode"] :=
    ( "Info: Delete all cells but Input and Code (including Print cells) in the selected notebook and contents in the Messages notebook ";
    FrontEndTokenExecute @ "DeleteGeneratedCells";
    NotebookDelete @ Cells[ CellStyle -> (Alternatives@@Complement[ Union@Flatten@Experimental`CellStyleNames[], {"Input", "Code"}])];
    NotebookDelete @ Cells @ MessagesNotebook[];
    );

Shortcut["QuitFrontEnd", enb_:EvaluationNotebook[]] :=
    ( "Info: quit the FrontEnd including closing all Untitled notebooks";
    Block[{StartTime, ToFileTime, Id, mproc},
    (* the restarting is done throu the OS. On MacOSX and Linux there needs to be a grace time *)
		If[ ("FileName" /. NotebookInformation[enb]) === "FileName" ,
    	    If[$OperatingSystem === "Unix",
     			(* since othwerwise on Linux restarting does not work ... , do: *)
				NotebookSave[enb, FileNameJoin[{$TemporaryDirectory, "shortcuttmp.nb"}]]
    	    ]
		];
	(* close all Untitled notebooks : *)
	Map[NotebookClose, Select[Notebooks[], ReplaceAll["FileName",NotebookInformation[#]] === "FileName"&]];
	(* save all left notebooks except the Messages one: *)
	Map[NotebookSave,  Select[Notebooks[],Replace["WindowTitle",NotebookInformation[#]]=!="Messages"&]];
	(* kill the newest FrontEnd operating system depently: *)
	Switch[ $OperatingSystem ,
	"Windows"
    	,    (* use NETLink to get a list of PID's and take the last one to be killed *)
		Needs["NETLink`"]; Symbol["NETLink`NETNew"]["System.Diagnostics.Process"];
		mproc = Select[System`Diagnostics`Process`GetProcessesByName["Mathematica"], Quiet[#@StartTime]=!=$Failed &];
		ReadList["!taskkill /PID " <> 
			ToString[(Last@SortBy[mproc, 
			(* if there are processes by other users we might get Access Denied, so we skip them: *)	
			#@StartTime @ ToFileTime[]&] ) @ Id ] <> " /f  " ,String
		];
    	,
	"Unix"
	    , 
		Run["kill -9 " <> ToString[ Last[ Sort[ ReadList[ StringToStream[RunProcess[{"pidof", "Mathematica"}, "StandardOutput"]],Number]]] ] <>
        	";  rm -f /tmp/shortcuttmp.nb ;"
    	]
		,
	"MacOSX"
		,
		Run["kill -9 " <> 
          (* take the newest pid , without using awk : *)
          ToString[ Last[Import["!ps axc | grep Mathematica", "Table"][[All, 1]]] ] 
		]
(*Switch*)
	]
] (* Block*)
);


Shortcut["RestartFrontEnd", enb_:EvaluationNotebook[]] :=
    ( "Info: restart the FrontEnd including closing all Untitled notebooks and potentially repoen the already saved notebook from 
             where this shortcut is executed";
    Block[{reopenfilename = "", StartTime, ToFileTime, Id, mproc},
    (* the restarting is done throu the OS. On MacOSX and Linux there needs to be a grace time,
        and on Windows 7 it is also a good idea *)
	With[{wait = "0.5" (* seconds *)}, (* this is heuristically determined! You may have to change this on you computer! *)
		If[ ("FileName" /. NotebookInformation[enb]) === "FileName"
			,
			reopenfilename = "";
    	    If[$OperatingSystem === "Unix",
     		(* since othwerwise on Linux restarting does not work ... , do: *)
			NotebookSave[enb, FileNameJoin[{$TemporaryDirectory, "shortcuttmp.nb"}]]
    	    ]
			,
			reopenfilename = "\"" <> ToFileName["FileName" /. NotebookInformation[enb]] <> "\""
		];
	(* close all Untitled notebooks : *)
	Map[NotebookClose, Select[Notebooks[], ReplaceAll["FileName",NotebookInformation[#]] === "FileName"&]];
	(* save all left notebooks except the Messages one: *)
	Map[NotebookSave,  Select[Notebooks[],Replace["WindowTitle",NotebookInformation[#]]=!="Messages"&]];
	(* kill the newest FrontEnd operating system depently: *)
	Switch[ $OperatingSystem ,
	"Windows"
    	,    (* use NETLink to get a list of PID's and take the last one to be killed *)
		Needs["NETLink`"]; Symbol["NETLink`NETNew"]["System.Diagnostics.Process"];
		mproc = Select[System`Diagnostics`Process`GetProcessesByName["Mathematica"], Quiet[#@StartTime]=!=$Failed &];
		ReadList["!taskkill /PID " <> 
		ToString[(Last@SortBy[mproc, 
		(* if there are processes by other users we might get Access Denied, so we skip them: *)	
		#@StartTime @ ToFileTime[]&] ) @ Id ] <> " /f & timeout /t 0.3 & start \"\" \"" <> 
		FileNameJoin[{$InstallationDirectory, "Mathematica.exe"}] <> "\" " <> reopenfilename
		  ,String];
    	,
	"Unix"
	    , 
		Run["kill -9 " <> ToString[ Last[ Sort[ ReadList[ StringToStream[RunProcess[{"pidof", "Mathematica"}, "StandardOutput"]],Number]]] ] <>
        	";  rm -f /tmp/shortcuttmp.nb ;sleep "<>wait<>"; '" <>  
        	FileNameJoin[{$InstallationDirectory, "Executables", "Mathematica"}]  <> "' " <> reopenfilename <> "& " 
    	]
		,
	"MacOSX"
		,
		Run["kill -9 " <> 
          (* take the newest pid , without using awk : *)
          ToString[ Last[Import["!ps axc | grep Mathematica", "Table"][[All, 1]]] ] <>
          "; sleep "<>wait<>"; open -a " <> FileNameTake[ParentDirectory[$InstallationDirectory], -1]  <> " " <> reopenfilename
		]
(*Switch*)
	]
]; (* With *)
] (* Block*)
);


Shortcut["RestartKernel"] :=
(
"Info: Based on an idea of Kuba.";

                    With[{ enb = SelectedNotebook[],
                             currentSetting = ToString@CurrentValue[$FrontEnd, "ClearEvaluationQueueOnKernelQuit"] },
                       Composition[
                              FrontEndExecute[FrontEndToken[#, "EvaluateNotebook"] ]&
                              ,
                              CreateDocument[#, Visible -> If[$OperatingSystem === "Windows",False, True], WindowSize -> {50, 50}, 
                                                WindowMargins -> {{-500, -500}, {-500, -500}},
                                                WindowTitle -> "restart"
                              ] &
                              ,
                              Map[ Function[z, Cell[StringReplace[z, "CURSET" -> currentSetting], "Input"]], #] &
                      ][
                      {
                      "CurrentValue[$FrontEndSession, \"ClearEvaluationQueueOnKernelQuit\"] = False; Quit[];"
                      ,
                       "CurrentValue[$FrontEndSession, \"ClearEvaluationQueueOnKernelQuit\"] = CURSET; NotebookClose[EvaluationNotebook[]]; $Line = 0; "
                      }
                      ] ;
                      SetSelectedNotebook[enb];
                     ]
);



(* This is a slight rewrite of Kuba's answer here: http://mathematica.stackexchange.com/a/55073/29
   However, it is not quite the same, which becomes apparent when evaluating a larger notebook 
   Therefore I prefer to user the version below using Shortcut["SelectToTop"]

Shortcut["EvaluateFromTop"] :=  Module[{i, enb = EvaluationNotebook[]},
  (* allow the cursor to be in between cells :*)
  If[CurrentValue[enb, "CellCount"] === 0, SelectionMove[enb, Previous, Cell, AutoScroll -> False]; ];
  (* Only go on if we are not at the top of the notebook: *)
  If[CurrentValue[enb, "CellCount"] > 0,
  	(* this **DOES NOT WORK*** , while it clearly should 
       SelectionMove[enb, All, Cell];
       Therefore this will all not work if the cursor is inside a cell ...
   *)
   Do[ SelectionMove[Experimental`FromCellIndex[enb, i], All, Cell, AutoScroll -> False];
       SelectionEvaluate[enb];
       ,{i, 1, Experimental`ToCellIndex @ SelectedCells[enb][[1]]}
   ];
   (* move explicitly out of the cell, otherwise the last cell might be selected ... *)
  If[CurrentValue[enb, "CellCount"] === 1, SelectionMove[enb, After, Cell, AutoScroll -> False]; ];
  ]];
*)
 
 
Shortcuts`Shortcut["EvaluateFromTop"] :=  ( 
Shortcut["SelectToTop"]; 
SelectionEvaluate[SelectedNotebook[]]
	);
  
(* Notice that from http://mathematica.stackexchange.com/a/55073/29
   I could not find a way to actually just select all cells,
   so I need to keep doing it the old way with CellTags and looping, 
   until the question gets truly answered. 
 *)
 
SetAttributes[Shortcut, HoldRest];
Shortcut["SelectToTop", lastCellFunction_: (CellEvaluationFunction :> Identity)] :=
            Module[{ enb = EvaluationNotebook[],
                     tag = StringJoin["tmp", ToString[Round[AbsoluteTime[]/$TimeUnit]]]
                   },
                If[ And[(enb =!= $Failed), Length[Cells[enb]] > 0 
                ],
               (*Iff the cursur is in between cells, move up one cell (with thank's to Kuba for the CellCount information) : *)
                If[CurrentValue[enb, "CellCount"] === 0,
                   SelectionMove[enb, Previous, Cell, AutoScroll -> False];
                ];
                (* Only go on if we are not at the top of the notebook: *)
                If[CurrentValue[enb, "CellCount"] > 0, (* add the unique tag to the last cell: *)
(*SelectionMove[enb, All, Cell, AutoScroll -> False];*)
                   If[lastCellFunction =!= (CellEvaluationFunction :> Identity),
                      SetOptions[NotebookSelection[enb], lastCellFunction]
                   ];
                   MathLink`CallFrontEnd[FrontEnd`SelectionAddCellTags[enb, {tag}]];
                   SelectionMove[enb, Before, Notebook, AutoScroll -> False];
                   SelectionMove[enb, Next, Cell, AutoScroll -> False];
                   (* add the cell tag to all cells up to the last one which we tagged above *)
                   While[FreeQ[CellTags /. Options[NotebookSelection[enb]], tag],
                         MathLink`CallFrontEnd[FrontEnd`SelectionAddCellTags[enb, {tag}]];
                         SelectionMove[enb, Next, Cell, AutoScroll -> False];
                ];
                (* select all cells with tag *)
                NotebookFind[enb, tag, All, CellTags, AutoScroll -> False];
                ];
                (* remove the tags *)
                MathLink`CallFrontEnd[FrontEnd`SelectionRemoveCellTags[enb, {tag}]];
            ]
            ];
   
Shortcut["SelectToBottom"] := 
	       Module[{ enb = EvaluationNotebook[],
                   tag = StringJoin["ShortcutsTemp", ToString[Round[AbsoluteTime[]/$TimeUnit]]]
                   },
                If[
                   (enb =!= $Failed) && Length[Cells[enb]] > 0,
               (*Iff the cursur is in between cells, move down one cell (with thank's to Kuba for the CellCount information) : *)
                If[SameQ[CurrentValue[enb, "CellCount"] ,0],
                   SelectionMove[enb, Next, Cell, AutoScroll -> False]
                ];
                (* Only go on if we are not at the end of the notebook: *)
                If[CurrentValue[enb, "CellCount"] > 0,
                   (* add the unique tag to the last cell: *)
                   MathLink`CallFrontEnd[FrontEnd`SelectionAddCellTags[enb, {tag}]];
                   (* and move to the end of the notebook: *)
                   SelectionMove[enb, After, Notebook, AutoScroll -> False];
                   (* on to the last cell *)
                   SelectionMove[enb, Previous, Cell, AutoScroll -> False];
                   (* add the cell tag to all cells up to the first one which we tagged above *)
                   While[FreeQ[ReplaceAll[CellTags, Options[NotebookSelection[enb]]], tag],
                         MathLink`CallFrontEnd[FrontEnd`SelectionAddCellTags[enb, {tag}]];
                         SelectionMove[enb, Previous, Cell, AutoScroll -> False];
                ];
                (* select all cells with tag *)
                NotebookFind[enb, tag, All, CellTags, AutoScroll -> False];
                ];
                (* remove the tags *)
                MathLink`CallFrontEnd[FrontEnd`SelectionRemoveCellTags[enb, {tag}]];
            ]
            ]
            
Shortcut["CloseUntitledNotebooks"] := 
        Map[NotebookClose, 
        	Select[Notebooks[],
                   StringMatchQ[ "WindowTitle" /. NotebookInformation[#], "Untitled-*"]&]];
                   
(* http://mathematica.stackexchange.com/questions/73212/how-to-move-a-cell-up-or-down-with-a-keystroke *)                   


Shortcut["MoveCellsUp"] := 
  With[{nb = SelectedNotebook[]},
   (NotebookWrite[nb, #2];
      SelectionMove[nb, Previous, Cell]
   ) &[
    SelectionMove[nb, All, Cell],
    NotebookRead[SelectedCells[nb]],
    NotebookDelete[nb],
    SelectionMove[nb, #, Cell] & /@ {Previous, Before}
    ]
  ];
  
Shortcut["MoveCellsDown"] :=  
  With[{nb = SelectedNotebook[]},
   (NotebookWrite[nb, #2];
      SelectionMove[nb, Previous, Cell]) &[
    SelectionMove[nb, All, Cell],
    NotebookRead[SelectedCells[nb]],
    NotebookDelete[nb],
    SelectionMove[nb, #, Cell] & /@ {Next, After}
    ]
  ];
  

  (*
status[s__]:= SetOptions[EvaluationNotebook[], WindowStatusArea -> StringJoin[ToString[#,InputForm]&/@{s}]];
Shortcut["DoIt"] := (
	status[DateString[]," start Deleting Output / Print / Message cells"];
	NotebookDelete @ Cells[ CellStyle -> ("Output"|"Print"|"Message")]; (* usually this makes sense *)
	NotebookDelete @ Cells @ MessagesNotebook[]; (* and this too *)
	status[DateString[], " Selecting all cells upewards."];
	If[("FileName" /. NotebookInformation[EvaluationNotebook[]]) =!= "FileName", 
		Shortcut["SelectToTop", CellEvaluationFunction :> ( (
	    NotebookWrite[EvaluationCell[], (NotebookRead[EvaluationCell[]] /. (CellEvaluationFunction :> _ ) :> Sequence[]), All ];
	    SelectionEvaluateCreateCell[EvaluationNotebook[]];
		NotebookSave[EvaluationNotebook[]]; 
		)&)
		],
		Shortcut["SelectToTop"]
	];
	status["restart the kernel."];
	Shortcut["RestartKernel"]; (* restart the kernel *)
	status[DateString[]<> " Evaluate the selected cells."];
	SelectionEvaluate[EvaluationNotebook[]]; (* evaluate all *)
	status[DateString[]];
);
*)


Shortcut["TestRun1"] := (
	"Info: delete all output, select input cells from the top until the cursor and evaluate the selected cells";
	NotebookDelete @ Cells[ CellStyle -> ("Output"|"Print"|"Message")]; (* usually this makes sense *)
	NotebookDelete @ Cells @ MessagesNotebook[]; (* and this too *)
	Shortcut["SelectToTop"];   (* select all cells from the cursor up to the beginning *)
	SelectionEvaluate[EvaluationNotebook[]]; (* evaluate all selected *)
);



Shortcut["TestRun2"] :=
    (
    "Info: delete all output, select input cells from the top until the cursor, restart the kernel and evaluate the selected cells";
    NotebookDelete @ Cells[ CellStyle -> ("Output"|"Print"|"Message")]; (* usually this makes sense *)
    NotebookDelete @ Cells @ MessagesNotebook[]; (* and this too *)
    Shortcut["SelectToTop"];   (* select all cells from the cursor up to the beginning *)
    If[$OperationgSystem =!= "Windows", Pause[.2]];
    Shortcut["RestartKernel"]; (* restart the kernel *)
    If[$OperationgSystem =!= "Windows", Pause[.2]];
    SelectionEvaluate[EvaluationNotebook[]]; (* evaluate all selected *)
    );

Shortcut["F6"] :=
    (
    "Info: stack windows ";
    FrontEndTokenExecute@"StackWindows"
    );

Shortcut["RunJoker"] :=
    (
    "Info: execute joker.m, a user configurable file";
    (If[ FileExistsQ[#1],
         Get[#1]
     ] &)[
     FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", Switch[$OperatingSystem,
    "MacOSX", "Macintosh",
    "Windows", "Windows",
    "Unix", "X"], "joker.m"}]]
    );

Shortcut["OpenJoker"] :=
    (
    "Info: open joker.m, a user configurable file";
    (If[ FileExistsQ[#1],
         NotebookOpen[#1]
     ] & )[FileNameJoin[{$UserBaseDirectory, "SystemFiles", "FrontEnd", "TextResources", 
    Switch[$OperatingSystem, "MacOSX", "Macintosh", "Windows", "Windows", "Unix", "X"], "joker.m"}]]
    );
    
Shortcut["EvaluateSelected"] :=
    (
    "Info: evaluate selected expression in a new notebook";
    CreateDocument[(If[ MatchQ[#1, _String] || MatchQ[#1, _RowBox],
                        {Cell[BoxData[#1], "Input"], 
                        ExpressionCell[ToExpression[#1], "Output"]},
                        Cell[TextData["Please select an expression inside a cell."], "Text"]
                    ] & )[
    Replace[NotebookRead[SelectedNotebook[]], {} -> Null]], WindowSize -> {Medium, FitAll}, 
    WindowMargins -> {{Automatic, 2}, {Automatic, 2}}, Magnification -> 1.5]
    );
    
Shortcut["OpenInit.m"] :=  NotebookOpen@FindFile["init.m"];

Shortcut["OpenUserBaseDirectory"] :=  SystemOpen[$UserBaseDirectory];

Shortcut["OpenShortcutHelpPage"] := ( 
                                      NotebookOpen["paclet:Shortcuts/ref/Shortcuts", 
               												 WindowSize -> {Scaled[1/2], Scaled[1]},
						              						 WindowMargins -> {{0, Automatic}, {Automatic, 0}}
									                    ]
									                  );
    
End[];

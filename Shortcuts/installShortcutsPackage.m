
(* If your firewall permits it you may install the Shortcuts` packge from a fresh Mathematica notebook like this: *)

(*

Get @ "http://goo.gl/aAxplX" 

or

Get @ "https://raw.githubusercontent.com/rolfmertig/Shortcuts/master/Shortcuts/installShortcutsPackage.m"
*)

(* If your firewall does not permit this, just download Shortcuts.zip  and install it to $UserBaseDirectory/Applications *)

Module[{tmpzip = FileNameJoin[{$TemporaryDirectory, "Shortcuts.zip"}],
	    gitzip = "https://raw.githubusercontent.com/rolfmertig/Shortcuts/master/Shortcuts.zip"
	   },
        If[FileExistsQ[tmpzip], DeleteFile[tmpzip]];
        URLSave[gitzip, tmpzip];
		If[!FileExistsQ[ tmpzip ], 
			Print["Installation failed!"]
			,
			Quiet @ DeleteDirectory[ FileNameJoin[{$UserBaseDirectory, "Applications", "Shortcuts"}], DeleteContents -> True];
        	ExtractArchive[tmpzip, FileNameJoin[{$UserBaseDirectory, "Applications"}] ]
		];
       Needs["Shortcuts`"]; 
       Shortcuts`InstallShortcuts[]
]
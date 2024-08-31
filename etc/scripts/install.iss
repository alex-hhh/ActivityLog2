;; This file is part of ActivityLog2, an fitness activity tracker
;; Copyright (C) 2016, 2024 Alex Harsanyi (AlexHarsanyi@gmail.com)
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.

;; Inno setup file to generate windows installer for ActivityLog2.

#define MyAppName "ActivityLog2"
#ifndef MyAppVersion
#define MyAppVersion "0.0"
#endif
#define MyAppPublisher "Alex Harsanyi"
#define MyAppURL "https://github.com/alex-hhh/ActivityLog2"
#define MyAppExeName "ActivityLog2.exe"

#define MyAppNameCA "AL2 Climb Analysis"
#define MyAppExeNameCA "AL2-Climb-Analysis.exe"


[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{75BDBA0F-4DEE-41FC-BFB1-8FDC38AF7665}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
;AppVerName={#MyAppName} {#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}
AppUpdatesURL={#MyAppURL}
AppCopyright=Copyright (C) 2022 Alex Harsanyi
AppComments=Analyze data from swim bike and run activities
DefaultDirName={autopf64}\{#MyAppName}
DefaultGroupName={#MyAppName}
AllowNoIcons=yes
OutputBaseFilename=ActivityLog2Setup-{#MyAppVersion}
Compression=lzma
SolidCompression=yes
LicenseFile=..\..\LICENSE
OutputDir=..\..\
PrivilegesRequired=lowest
PrivilegesRequiredOverridesAllowed=commandline dialog

;; NOTE: UsePreviousPrivileges=yes (the default) will cause a second install
;; of this application to no longer prompt the user whether to re-install for
;; the current user or all users.

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked

[Files]
Source: "..\..\dist\ActivityLog2.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\dist\AL2-Climb-Analysis.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\dist\AL2-Activity-Import.exe"; DestDir: "{app}"; Flags: ignoreversion
Source: "..\..\dist\lib\*"; DestDir: "{app}/lib"; Flags: ignoreversion recursesubdirs createallsubdirs
Source: "..\..\LICENSE"; DestDir: "{app}"; DestName: "LICENSE.txt"
Source: "..\..\docs\CHANGES.md"; DestDir: "{app}"; DestName: "README.txt"; Flags: isreadme
Source: "..\..\dist\manifest-sha256.txt"; DestDir: "{app}"
Source: "..\..\dist\manifest-sha256.sig"; DestDir: "{app}"; Flags: skipifsourcedoesntexist

;; NOTE: Don't use "Flags: ignoreversion" on any shared system files

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{#MyAppNameCA}"; Filename: "{app}\{#MyAppExeNameCA}"
Name: "{group}\{cm:ProgramOnTheWeb,{#MyAppName}}"; Filename: "{#MyAppURL}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"

;; The icon is always placed on the users desktop, rather than the common
;; desktop, even if the application is installed for all users.

Name: "{userdesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

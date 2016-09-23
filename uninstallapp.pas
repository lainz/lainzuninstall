unit UninstallApp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UninstallEntries, Registry, LazUTF8;

type

  { TUninstallApp }

  TUninstallApp = class(TComponent)
  private
    FEntries: TUninstallEntries;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  private
    function ReadString(const reg: TRegistry; const aName: string): string;
    function ReadInteger(const reg: TRegistry; const aName: string): integer;
    procedure LoadEntries(const Access: longword; const Root: HKEY);
  public
    procedure LoadData();
    property Entries: TUninstallEntries read FEntries;
  end;

const
  UNINSTALL_KEY = '\Software\Microsoft\Windows\CurrentVersion\Uninstall\';

implementation

{ TUninstallApp }

function CompareEntriesByName(const Item1, Item2: TUninstallEntry): integer;
begin
  Result := UTF8CompareStr(Item1.DisplayName, Item2.DisplayName);
end;

constructor TUninstallApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEntries := TUninstallEntries.Create();
end;

destructor TUninstallApp.Destroy;
begin
  FEntries.Free;
  inherited Destroy;
end;

function TUninstallApp.ReadString(const reg: TRegistry; const aName: string): string;
begin
  Result := '';
  try
    if reg.ValueExists(aName) then
      case reg.GetDataType(aName) of
        rdString: Result := reg.ReadString(aName);
        rdInteger: Result := IntToStr(reg.ReadInteger(aName));
      end;
  except
  end;
end;

function TUninstallApp.ReadInteger(const reg: TRegistry; const aName: string): integer;
begin
  Result := -1;
  try
    case reg.GetDataType(aName) of
      rdString: Result := StrToIntDef(reg.ReadString(aName), -1);
      rdInteger: Result := reg.ReadInteger(aName);
    end;
  except
  end;
end;

procedure TUninstallApp.LoadEntries(const Access: longword; const Root: HKEY);
var
  i, j: integer;
  key_names: TStringList;
  reg: TRegistry;
begin
  reg := TRegistry.Create(Access);
  reg.RootKey := Root;

  if reg.OpenKeyReadOnly(UNINSTALL_KEY) then
  begin
    key_names := TStringList.Create;
    reg.GetKeyNames(key_names);
  end;

  for i := 0 to {%H-}key_names.Count - 1 do
  begin
    if reg.OpenKeyReadOnly(UNINSTALL_KEY + key_names[i]) then
    begin
      j := Entries.Add(TUninstallEntry.Create());
      with TUninstallEntry(Entries.Items[j]) do
      begin
        GUID := key_names[i];
        ParentKeyName := ReadString(reg, 'ParentKeyName');
        ReleaseType := ReadString(reg, 'ReleaseType');
        SystemComponent := ReadInteger(reg, 'SystemComponent');
        AuthorizedCDFPrefix := ReadString(reg, 'AuthorizedCDFPrefix');
        Comments := ReadString(reg, 'Comments');
        Contact := ReadString(reg, 'Contact');
        DisplayName := ReadString(reg, 'DisplayName');
        DisplayVersion := ReadString(reg, 'DisplayVersion');
        EstimatedSize := ReadInteger(reg, 'EstimatedSize');
        HelpLink := ReadString(reg, 'HelpLink');
        HelpTelephone := ReadString(reg, 'HelpTelephone');
        InstallDate := ReadString(reg, 'InstallDate');
        InstallLocation := ReadString(reg, 'InstallLocation');
        InstallSource := ReadString(reg, 'InstallSource');
        Language := ReadInteger(reg, 'Language');
        ModifyPath := ReadString(reg, 'ModifyPath');
        Publisher := ReadString(reg, 'Publisher');
        Readme := ReadString(reg, 'Readme');
        SettingIdentifier := ReadString(reg, 'SettingIdentifier');
        Size := ReadString(reg, 'Size');
        UninstallString := ReadString(reg, 'UninstallString');
        URLInfoAbout := ReadString(reg, 'URLInfoAbout');
        URLUpdateInfo := ReadString(reg, 'URLUpdateInfo');
        Version := ReadInteger(reg, 'Version');
        VersionMajor := ReadInteger(reg, 'VersionMajor');
        VersionMinor := ReadInteger(reg, 'VersionMinor');
        WindowsInstaller := ReadInteger(reg, 'WindowsInstaller');
      end;
    end;
  end;

  key_names.Free;
  reg.Free;
end;

procedure TUninstallApp.LoadData;
begin
  Entries.Clear;
  // This is tested only under 64 bit OS
  LoadEntries(KEY_WOW64_32KEY, HKEY_LOCAL_MACHINE);
  LoadEntries(KEY_WOW64_64KEY, HKEY_LOCAL_MACHINE);
  LoadEntries(KEY_WRITE, HKEY_CURRENT_USER);
  Entries.Sort(@CompareEntriesByName);
end;

end.




unit UninstallEntries;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FGL;

type

  { TUninstallEntry }

  TUninstallEntry = class(TObject)
  private
    // FDisplayIcon: string;
    // FNoModify: Integer;
    // FNoRepair: Integer;
    // ShellProducts: string;
    FAuthorizedCDFPrefix: string;
    FComments: string;
    FContact: string;
    FDisplayName: string;
    FDisplayVersion: string;
    FEstimatedSize: integer;
    FGUID: string;
    FHelpLink: string;
    FHelpTelephone: string;
    FInstallDate: string;
    FInstallLocation: string;
    FInstallSource: string;
    FLanguage: integer;
    FModifyPath: string;
    FParentKeyName: string;
    FPublisher: string;
    FReadme: string;
    FReleaseType: string;
    FSettingIdentifier: string;
    FSize: string;
    FSystemComponent: integer;
    FUninstallString: string;
    FURLInfoAbout: string;
    FURLUpdateInfo: string;
    FVersion: integer;
    FVersionMajor: integer;
    FVersionMinor: integer;
    FWindowsInstaller: integer;
    procedure SetFAuthorizedCDFPrefix(AValue: string);
    procedure SetFComments(AValue: string);
    procedure SetFContact(AValue: string);
    procedure SetFDisplayName(AValue: string);
    procedure SetFDisplayVersion(AValue: string);
    procedure SetFEstimatedSize(AValue: integer);
    procedure SetFGUID(AValue: string);
    procedure SetFHelpLink(AValue: string);
    procedure SetFHelpTelephone(AValue: string);
    procedure SetFInstallDate(AValue: string);
    procedure SetFInstallLocation(AValue: string);
    procedure SetFInstallSource(AValue: string);
    procedure SetFLanguage(AValue: integer);
    procedure SetFModifyPath(AValue: string);
    procedure SetFParentKeyName(AValue: string);
    procedure SetFPublisher(AValue: string);
    procedure SetFReadme(AValue: string);
    procedure SetFReleaseType(AValue: string);
    procedure SetFSettingIdentifier(AValue: string);
    procedure SetFSize(AValue: string);
    procedure SetFSystemComponent(AValue: integer);
    procedure SetFUninstallString(AValue: string);
    procedure SetFURLInfoAbout(AValue: string);
    procedure SetFURLUpdateInfo(AValue: string);
    procedure SetFVersion(AValue: integer);
    procedure SetFVersionMajor(AValue: integer);
    procedure SetFVersionMinor(AValue: integer);
    procedure SetFWindowsInstaller(AValue: integer);
  published
    property AuthorizedCDFPrefix: string read FAuthorizedCDFPrefix
      write SetFAuthorizedCDFPrefix;
    property Comments: string read FComments write SetFComments;
    property Contact: string read FContact write SetFContact;
    property DisplayName: string read FDisplayName write SetFDisplayName;
    property DisplayVersion: string read FDisplayVersion write SetFDisplayVersion;
    property EstimatedSize: integer read FEstimatedSize write SetFEstimatedSize;
    property HelpLink: string read FHelpLink write SetFHelpLink;
    property HelpTelephone: string read FHelpTelephone write SetFHelpTelephone;
    property InstallDate: string read FInstallDate write SetFInstallDate;
    property InstallLocation: string read FInstallLocation write SetFInstallLocation;
    property InstallSource: string read FInstallSource write SetFInstallSource;
    property Language: integer read FLanguage write SetFLanguage;
    property ModifyPath: string read FModifyPath write SetFModifyPath;
    property Publisher: string read FPublisher write SetFPublisher;
    property Readme: string read FReadme write SetFReadme;
    property SettingIdentifier: string read FSettingIdentifier
      write SetFSettingIdentifier;
    property Size: string read FSize write SetFSize;
    property UninstallString: string read FUninstallString write SetFUninstallString;
    property URLInfoAbout: string read FURLInfoAbout write SetFURLInfoAbout;
    property URLUpdateInfo: string read FURLUpdateInfo write SetFURLUpdateInfo;
    property Version: integer read FVersion write SetFVersion;
    property VersionMajor: integer read FVersionMajor write SetFVersionMajor;
    property VersionMinor: integer read FVersionMinor write SetFVersionMinor;
    property WindowsInstaller: integer read FWindowsInstaller write SetFWindowsInstaller;
  published
    property GUID: string read FGUID write SetFGUID;
    property ParentKeyName: string read FParentKeyName write SetFParentKeyName;
    property ReleaseType: string read FReleaseType write SetFReleaseType;
    property SystemComponent: integer read FSystemComponent write SetFSystemComponent;
  end;

  TUninstallEntries = specialize TFPGObjectList<TUninstallEntry>;

implementation

{ TUninstallEntry }

procedure TUninstallEntry.SetFAuthorizedCDFPrefix(AValue: string);
begin
  if FAuthorizedCDFPrefix = AValue then
    Exit;
  FAuthorizedCDFPrefix := AValue;
end;

procedure TUninstallEntry.SetFComments(AValue: string);
begin
  if FComments = AValue then
    Exit;
  FComments := AValue;
end;

procedure TUninstallEntry.SetFContact(AValue: string);
begin
  if FContact = AValue then
    Exit;
  FContact := AValue;
end;

procedure TUninstallEntry.SetFDisplayName(AValue: string);
begin
  if FDisplayName = AValue then
    Exit;
  FDisplayName := AValue;
end;

procedure TUninstallEntry.SetFDisplayVersion(AValue: string);
begin
  if FDisplayVersion = AValue then
    Exit;
  FDisplayVersion := AValue;
end;

procedure TUninstallEntry.SetFEstimatedSize(AValue: integer);
begin
  if FEstimatedSize = AValue then
    Exit;
  FEstimatedSize := AValue;
end;

procedure TUninstallEntry.SetFGUID(AValue: string);
begin
  if FGUID = AValue then
    Exit;
  FGUID := AValue;
end;

procedure TUninstallEntry.SetFHelpLink(AValue: string);
begin
  if FHelpLink = AValue then
    Exit;
  FHelpLink := AValue;
end;

procedure TUninstallEntry.SetFHelpTelephone(AValue: string);
begin
  if FHelpTelephone = AValue then
    Exit;
  FHelpTelephone := AValue;
end;

procedure TUninstallEntry.SetFInstallDate(AValue: string);
begin
  if FInstallDate = AValue then
    Exit;
  FInstallDate := AValue;
end;

procedure TUninstallEntry.SetFInstallLocation(AValue: string);
begin
  if FInstallLocation = AValue then
    Exit;
  FInstallLocation := AValue;
end;

procedure TUninstallEntry.SetFInstallSource(AValue: string);
begin
  if FInstallSource = AValue then
    Exit;
  FInstallSource := AValue;
end;

procedure TUninstallEntry.SetFLanguage(AValue: integer);
begin
  if FLanguage = AValue then
    Exit;
  FLanguage := AValue;
end;

procedure TUninstallEntry.SetFModifyPath(AValue: string);
begin
  if FModifyPath = AValue then
    Exit;
  FModifyPath := AValue;
end;

procedure TUninstallEntry.SetFParentKeyName(AValue: string);
begin
  if FParentKeyName = AValue then
    Exit;
  FParentKeyName := AValue;
end;

procedure TUninstallEntry.SetFPublisher(AValue: string);
begin
  if FPublisher = AValue then
    Exit;
  FPublisher := AValue;
end;

procedure TUninstallEntry.SetFReadme(AValue: string);
begin
  if FReadme = AValue then
    Exit;
  FReadme := AValue;
end;

procedure TUninstallEntry.SetFReleaseType(AValue: string);
begin
  if FReleaseType = AValue then
    Exit;
  FReleaseType := AValue;
end;

procedure TUninstallEntry.SetFSettingIdentifier(AValue: string);
begin
  if FSettingIdentifier = AValue then
    Exit;
  FSettingIdentifier := AValue;
end;

procedure TUninstallEntry.SetFSize(AValue: string);
begin
  if FSize = AValue then
    Exit;
  FSize := AValue;
end;

procedure TUninstallEntry.SetFSystemComponent(AValue: integer);
begin
  if FSystemComponent = AValue then
    Exit;
  FSystemComponent := AValue;
end;

procedure TUninstallEntry.SetFUninstallString(AValue: string);
begin
  if FUninstallString = AValue then
    Exit;
  FUninstallString := AValue;
end;

procedure TUninstallEntry.SetFURLInfoAbout(AValue: string);
begin
  if FURLInfoAbout = AValue then
    Exit;
  FURLInfoAbout := AValue;
end;

procedure TUninstallEntry.SetFURLUpdateInfo(AValue: string);
begin
  if FURLUpdateInfo = AValue then
    Exit;
  FURLUpdateInfo := AValue;
end;

procedure TUninstallEntry.SetFVersion(AValue: integer);
begin
  if FVersion = AValue then
    Exit;
  FVersion := AValue;
end;

procedure TUninstallEntry.SetFVersionMajor(AValue: integer);
begin
  if FVersionMajor = AValue then
    Exit;
  FVersionMajor := AValue;
end;

procedure TUninstallEntry.SetFVersionMinor(AValue: integer);
begin
  if FVersionMinor = AValue then
    Exit;
  FVersionMinor := AValue;
end;

procedure TUninstallEntry.SetFWindowsInstaller(AValue: integer);
begin
  if FWindowsInstaller = AValue then
    Exit;
  FWindowsInstaller := AValue;
end;

end.

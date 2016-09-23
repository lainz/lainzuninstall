unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  UninstallApp;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    StringGrid1: TStringGrid;
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DblClick(Sender: TObject);
  private
    FApps: TUninstallApp;
    procedure Fill();
    procedure Reload();
    function GetFormattedSize(size: int64): string;
    function GetFormattedDate(Value: string): string;
    procedure ExecuteUninstall(const UninstallData: string);
  end;

var
  frmMain: TfrmMain;

implementation

uses
  Process, UTF8Process;

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FApps := TUninstallApp.Create(Self);
  Reload();
end;

procedure TfrmMain.StringGrid1DblClick(Sender: TObject);
begin
  ExecuteUninstall(StringGrid1.Cells[5, StringGrid1.Row]);
end;

procedure TfrmMain.Fill;
var
  i, j: integer;
begin
  StringGrid1.BeginUpdate;
  StringGrid1.RowCount := 1;
  StringGrid1.Clear;
  for i := 0 to FApps.Entries.Count - 1 do
  begin
    with FApps.Entries.Items[i] do
    begin
      { Display only these entries }
      if (ParentKeyName = '') and (DisplayName <> '') then
      begin
        j := StringGrid1.RowCount;
        StringGrid1.RowCount := StringGrid1.RowCount + 1;
        StringGrid1.Cells[0, j] := DisplayName;
        StringGrid1.Cells[1, j] := Publisher;
        StringGrid1.Cells[2, j] := GetFormattedDate(InstallDate);
        StringGrid1.Cells[3, j] := GetFormattedSize(EstimatedSize);
        StringGrid1.Cells[4, j] := DisplayVersion;
        if UninstallString <> '' then
          StringGrid1.Cells[5, j] := UninstallString
        else if (GUID[1] = '{') then
          StringGrid1.Cells[5, j] := 'msiexec.exe /X' + GUID;
      end;
    end;
  end;
  { Hide that column, we need it only to access the UninstallString data }
  StringGrid1.Columns[5].Visible := False;
  StringGrid1.AutoSizeColumns;
  StringGrid1.EndUpdate();
end;

procedure TfrmMain.Reload;
begin
  FApps.LoadData();
  Fill();
end;

function TfrmMain.GetFormattedSize(size: int64): string;
begin
  Result := '';
  if size >= 0 then
  begin
    if (size < 1024) then
      Result := FloatToStrF(size, ffNumber, 2, 2) + ' KB'
    else
      Result := FloatToStrF(size / 1024, ffNumber, 2, 2) + ' MB';
  end;
end;

function TfrmMain.GetFormattedDate(Value: string): string;
begin
  Result := '';
  if Length(Value) = 8 then
  begin
    Insert(DefaultFormatSettings.DateSeparator, Value, 7);
    Insert(DefaultFormatSettings.DateSeparator, Value, 5);
    Result := Value;
  end;
end;

procedure TfrmMain.ExecuteUninstall(const UninstallData: string);
var
  proc: TProcessUTF8;
begin
  try
    proc := TProcessUTF8.Create(Self);
    proc.Executable := 'cmd.exe /C "' + UninstallData + '"';
    //proc.ShowWindow := swoHIDE;
    try
      proc.Execute;
    except
      ShowMessage('Can not run uninstall.' + LineEnding + UninstallData);
    end;
  finally
    proc.Free;
  end;
end;

end.

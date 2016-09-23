unit WindowsBitness;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Windows;

function IsWindows64: boolean;

implementation

function IsWindows64: boolean;
{
Detect if we are running on 64 bit Windows or 32 bit Windows,
independently of bitness of this program.
Original source:
http://www.delphipraxis.net/118485-ermitteln-ob-32-bit-oder-64-bit-betriebssystem.html
modified for FreePascal in German Lazarus forum:
http://www.lazarusforum.de/viewtopic.php?f=55&t=5287
}
{$ifdef WIN32} //Modified KpjComp for 64bit compile mode
type
  TIsWow64Process = function( // Type of IsWow64Process API fn
      Handle: Windows.THandle; var Res: Windows.BOOL): Windows.BOOL; stdcall;
var
  IsWow64Result: Windows.BOOL; // Result from IsWow64Process
  IsWow64Process: TIsWow64Process; // IsWow64Process fn reference
begin
  // Try to load required function from kernel32
  IsWow64Process := TIsWow64Process(Windows.GetProcAddress(
    Windows.GetModuleHandle('kernel32'), 'IsWow64Process'));
  if Assigned(IsWow64Process) then
  begin
    // Function is implemented: call it
    if not IsWow64Process(Windows.GetCurrentProcess, IsWow64Result) then
      raise SysUtils.Exception.Create('IsWindows64: bad process handle');
    // Return result of function
    Result := IsWow64Result;
  end
  else
    // Function not implemented: can't be running on Wow64
    Result := False;
{$else} //if were running 64bit code, OS must be 64bit :)
begin
 Result := True;
{$endif}
end;

end.


unit SardConsole;
{**
 *  This file is part of the "SARD"
 *
 * @license   The MIT License (MIT)
 *            Included in this distribution
 * @author    Zaher Dirkey
 *}

{$IFDEF FPC}
{$mode delphi}
{$WARN 5024 off : Parameter "$1" not used}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  mnUtils,
  sardClasses, sardObjects, sardParsers, sardScripts, sardJSONs;

type

  { TSardConsole }

  TSardConsole = class(TObject)
  protected
    FParams: TStringList;
    Verbose: Boolean;
  public
    procedure Run;
    constructor Create;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
    function WaitKey: Boolean;
  end;

implementation

{ TSardApplication }

procedure TSardConsole.Run;
var
  ErrorMsg: String;
  Script: TCodeScript;
  Lines: TStringList;
  FileName: string;
  FileNames: TArray<String>;
begin
  if GetArgumentSwitch(FParams, 'h', 'help') then
  begin
    WriteHelp;
    Exit;
  end;

  Verbose := GetArgumentSwitch(FParams, 'v', 'verbose');

  try
    if ParamCount > 0 then
    begin
      GetArgument(FParams, FileNames);
      if Length(FileNames) > 0 then
        FileName := FileNames[0];
      if (FileName <> '') and FileExists(FileName) then
      begin
        if Verbose then
          Writeln('Loading file: ' + FileName);
        Script := TCodeScript.Create;
        try
          Lines := TStringList.Create;
          try
            Lines.LoadFromFile(FileName);
            //Lines.LoadFromFile(Location + 'test.sard');
            //Lines.Text := 'x:{:=10};';
            //Lines.Text := 'print(10);print(20)';
            Script.RegisterInternals := False;
            if Verbose then
              Writeln('Compile');
            Script.Compile(Lines);

            if GetArgumentSwitch(FParams, 'e') then
            begin
              if Verbose then
                Writeln('Export:');
              Script.ExportToConsole;
            end;

            if Verbose then
              Writeln('Run');
            Script.Run;

            if Verbose then
              Writeln('Result:');
            WriteLn((Script as TCodeScript).Result);
            //Script.ExportToFile('export.sard');
          finally
            FreeAndNil(Lines);
          end;
        finally
          FreeAndNil(Script);
        end;
      end
      else
        WriteLn('File script not exists');
    end;
  except
    on E:Exception do
    begin
      WriteLn(E.Message);
//      raise;
    end;
  end;
end;

constructor TSardConsole.Create;
begin
  inherited Create;
  FParams := TStringList.Create;
  ParseCommandLine(CmdLine, FParams);
end;

destructor TSardConsole.Destroy;
begin
  FreeAndNil(FParams);
  inherited Destroy;
end;

function TSardConsole.WaitKey: Boolean;
begin
  Result := GetArgumentSwitch(FParams, 'w', 'wait');
end;

procedure TSardConsole.WriteHelp;
begin
  writeln('Usage: sard -h');
end;

end.

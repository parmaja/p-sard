unit sard;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0 (modified of http://www.gnu.org/licenses/lgpl.html)
 *            included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}

{TODO:
  Check S is not empty before push
  Push with the process class and id
}

interface

uses
  Classes, SysUtils, contnrs,
  mnStreams;

const
  sEOL = [#0, #13, #10];
  sWhitespace = [' ', #9, #13, #10];

type
  EsardException = class(Exception)
  private
    FLine: Integer;
    FColumn: Integer;
    FCode: Cardinal;
  public
    constructor Create(const Msg: string); overload;
    constructor Create(const Msg: string; const Column, Line: Integer); overload;
    property Code: Cardinal read FCode write FCode;
    property Column: Integer read FColumn write FColumn;
    property Line: Integer read FLine write FLine;
  end;

  EsardParserException = class(EsardException);

  TsardFeeder = class(TObject)
  private
    FActive: Boolean;
    FOwned: Boolean;
    FVersion: string;
    FStream: TmnWrapperStream;
    FHeader: TStringList;
    FCharset: string;
    FStandalone: Boolean;
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(Stream: TmnWrapperStream; Owned: Boolean = True); overload;
    constructor Create(const FileName:string); overload;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property Active: Boolean read FActive write FActive;
    property Stream: TmnWrapperStream read FStream;
    property Header: TStringList read FHeader write FHeader;
    property Standalone: Boolean read FStandalone write FStandalone;
    property Version: string read FVersion write FVersion;
    property Charset: string read FCharset write FCharset;
  end;

  sardBracketKind = (brParenthesis, brSquare, brCurly);// and (), [], {} or maybe <>
  sardTokinKind = (tkComment, tkIdentifier, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

  TsardProcessID = type Integer;

  TsardProcesses = class;
  TsardCustomScanner = class;
  TsardProcessClass = class of TsardProcess;
  TsardParser = class;

  { TsardProcess }

  TsardProcess = class(TObject)
  private
    FProcesses: TsardProcesses;
  protected
    function CheckText(S: string; const Text: string; const Column: Integer): Boolean;
    function ScanText(S: string; const Text: string; var Column: Integer): Boolean;
    procedure ScanTo(NextProcess: TsardProcessID; const SubStr, Text: string; var Column: Integer; const Line: Integer); virtual;
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer); virtual; abstract;
    procedure Open(vBracket: sardBracketKind); virtual;
    procedure Close(vBracket: sardBracketKind); virtual;
    procedure Terminate; virtual;
    procedure Push(Token: String; TokenID: Integer); virtual;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; virtual;
    function ChooseProcess(const Text: string; var Column: Integer; const Line: Integer): Integer;
    procedure ChangeProcess(NextProcess: TsardProcessID);
    procedure SelectProcess(ProcessClass: TsardProcessClass);
  public
    Index: TsardProcessID;
    Collected: string; //buffer
    Process: TsardProcessID;
    constructor Create(vProcesses: TsardProcesses); virtual;
    destructor Destroy; override;
    property Processes: TsardProcesses read FProcesses;
  end;

  { TsardProcesses }

  TsardProcesses = class(TObjectList)
  private
    FScanner: TsardCustomScanner;
    function GetItem(Index: Integer): TsardProcess;
  public
    constructor Create(vScanner: TsardCustomScanner; FreeObjects: boolean = True); virtual;
    function ChooseProcess(const Text: string; var Column: Integer; const Line: Integer): Integer;
    function Find(const ProcessClass: TsardProcessClass): TsardProcess;
    function RegisterProcess(ProcessClass: TsardProcessClass): TsardProcessID;
    property Items[Index: Integer]: TsardProcess read GetItem; default;
    property Scanner: TsardCustomScanner read FScanner;
  end;

  { TsardCustomScanner }

  TsardCustomScanner = class(TsardFeeder)
  private
    FParser: TsardParser;
    FProcess: TsardProcessID;
    FProcesses: TsardProcesses;
    procedure SetParser(AValue: TsardParser);
  protected
    FDefaultProcess: TsardProcessID; //Default process
    FOffProcess: TsardProcessID; //Fall off into it when no one accept it
    procedure ChangeProcess(NextProcess: TsardProcessID);
    procedure SelectProcess(ProcessClass: TsardProcessClass);

    function CreateParser:TsardParser; virtual; abstract;
    property Processes: TsardProcesses read FProcesses;
    property Parser: TsardParser read FParser write SetParser;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ScanLine(const Text: string; const Line: Integer);
    procedure Scan(const Lines: TStrings);
  end;

  { TsardParser }

  TsardParser = class(TObject)
  protected
    procedure Open(vBracket: sardBracketKind); virtual; abstract;
    procedure Close(vBracket: sardBracketKind); virtual; abstract;
    procedure Terminate; virtual; abstract;
    procedure Push(Token: String; TokenID: Integer); virtual; abstract;
  end;

implementation

uses
  StrUtils;

{ EmnXMLException }

constructor EsardException.Create(const Msg: string; const Column, Line: Integer);
begin
  Create(Msg + #13'Line Number ' + IntToStr(Line) + ', Column ' + IntToStr(Column));
  FLine := Line;
  FColumn := Column;
end;

constructor EsardException.Create(const Msg: string);
begin
  inherited Create(Msg);
end;

{ TsardProcess }

function TsardProcess.CheckText(S: string; const Text: string; const Column: Integer): Boolean;
begin
  Result := (Length(Text) - Column) >= length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Column, Length(S))) = LowerCase(S); //caseinsensitive
end;

function TsardProcess.ScanText(S: string; const Text: string; var Column: Integer): Boolean;
begin
  Result := (Length(Text) - Column) >= length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Column, Length(S))) = LowerCase(S); //caseinsensitive
  if Result then
    Column := Column + Length(S);
end;

procedure TsardProcess.ScanTo(NextProcess: TsardProcessID; const SubStr, Text: string; var Column: Integer; const Line: Integer);
var
  p: integer;
  l, c, i: integer;
begin
  p := 0;
  c := 1;
  l := Length(SubStr);
  for i := Column to Length(Text) do
  begin
    if not (Text[i] in sWhitespace) then
    begin
      if Text[i] = SubStr[c] then
      begin
        if c = l then
        begin
          p := i + 1;
          break;
        end;
        Inc(c);
      end
      else
        raise EsardParserException.Create('syntax error', Line, Column);
    end;
  end;

  if p > 0 then
  begin
    Column := p;
    ChangeProcess(NextProcess);
  end
  else
  begin
    Column := Length(Text) + 1;
    ChangeProcess(Process);
  end;
end;

procedure TsardProcess.Open(vBracket: sardBracketKind);
begin
  Processes.Scanner.Parser.Open(vBracket);
end;

procedure TsardProcess.Close(vBracket: sardBracketKind);
begin
  Processes.Scanner.Parser.Close(vBracket);
end;

procedure TsardProcess.Terminate;
begin
  Processes.Scanner.Parser.Terminate;
end;

procedure TsardProcess.Push(Token: String; TokenID: Integer);
begin
  Processes.Scanner.Parser.Push(Token, TokenID);
end;

function TsardProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;
end;

function TsardProcess.ChooseProcess(const Text: string; var Column: Integer; const Line: Integer): Integer;
begin
  Result := Processes.ChooseProcess(Text, Column, Line);
end;

procedure TsardProcess.ChangeProcess(NextProcess: TsardProcessID);
begin
  Processes.Scanner.ChangeProcess(NextProcess);
end;

procedure TsardProcess.SelectProcess(ProcessClass: TsardProcessClass);
begin
  Processes.Scanner.SelectProcess(ProcessClass);
end;

constructor TsardProcess.Create(vProcesses: TsardProcesses);
begin
  inherited Create;
  FProcesses := vProcesses;
end;

destructor TsardProcess.Destroy;
begin
  inherited Destroy;
end;

{ TsardProcesses }

function TsardProcesses.GetItem(Index: Integer): TsardProcess;
begin
  Result := inherited Items[Index] as TsardProcess;
end;

constructor TsardProcesses.Create(vScanner: TsardCustomScanner; FreeObjects: boolean);
begin
  FScanner := vScanner;
  inherited Create(FreeObjects);
end;

function TsardProcesses.ChooseProcess(const Text: string; var Column: Integer; const Line: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;//Scanner.FOffProcess;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Index <> Result) and Items[i].Accept(Text, Column, Line) then
    begin
      Result := i;
      break;
    end;
  end;
  if Result < 0 then
    raise EsardException.Create('Process not found:' + Text[Column]);
  Scanner.ChangeProcess(Result);
end;

function TsardProcesses.Find(const ProcessClass: TsardProcessClass): TsardProcess;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if ProcessClass = Items[i].ClassType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsardProcesses.RegisterProcess(ProcessClass: TsardProcessClass): TsardProcessID;
var
  aProcess: TsardProcess;
begin
  aProcess := ProcessClass.Create(Self);
  Result := Add(aProcess);
  aProcess.Index := Result;
end;

procedure TsardFeeder.Stop;
begin
  if not FActive then
    raise EsardException.Create('File already closed');
  DoStop;
  FActive := False;
end;

constructor TsardFeeder.Create(Stream: TmnWrapperStream; Owned: Boolean);
begin
  Create;
  if Stream = nil then
    raise EsardException.Create('Stream is nil');
  FStream := Stream;
  FOwned := Owned;
end;

constructor TsardFeeder.Create(const FileName: string);
begin
  Create(TmnWrapperStream.Create(TFileStream.Create(FileName, fmOpenRead)));
end;

constructor TsardFeeder.Create;
begin
  inherited;
  FHeader := TStringList.Create;
  FVersion := '1.0';
  {$ifdef FPC}
  FCharset := 'utf-8';
  {$else}
  FCharset := 'iso-8859-1';
  {$endif}
end;

destructor TsardFeeder.Destroy;
begin
{  if Active then
    Stop;}
  if FOwned then
    FStream.Free;
  FHeader.Free;
  inherited;
end;

procedure TsardFeeder.DoStop;
begin
end;

procedure TsardFeeder.DoStart;
begin
end;

procedure TsardFeeder.Start;
begin
  if FActive then
    raise EsardException.Create('File already opened');
  FActive := True;
  DoStart;
end;

{ TsardCustomScanner }

procedure TsardCustomScanner.SetParser(AValue: TsardParser);
begin
  if FParser = AValue then Exit;
  FParser := AValue;
end;

procedure TsardCustomScanner.ChangeProcess(NextProcess: TsardProcessID);
begin
  if FProcess <> NextProcess then
  begin
    if NextProcess = 0 then
      FProcess := FProcess;
    FProcess := NextProcess;
  end;
end;

procedure TsardCustomScanner.SelectProcess(ProcessClass: TsardProcessClass);
var
  aProcess: TsardProcess;
begin
  aProcess := Processes.Find(ProcessClass);
  if aProcess = nil then
    raise EsardException.Create('Process not found');
  ChangeProcess(aProcess.Index);
end;

constructor TsardCustomScanner.Create;
begin
  inherited Create;
  FProcesses := TsardProcesses.Create(Self);
  Parser := CreateParser;
end;

destructor TsardCustomScanner.Destroy;
begin
  FreeAndNil(FProcesses);
  inherited Destroy;
end;

procedure TsardCustomScanner.ScanLine(const Text: string; const Line: Integer);
var
  Column, OldColumn: Integer;
  OldProcess: TsardProcessID;
  l: Integer;
begin
  if not Active then
    raise EsardException.Create('Scanner not started');
  Column := 1; //start of pascal string is 1
  l := Length(Text);
  while (Column <= l) do
  begin
    OldColumn := Column;
    OldProcess := FProcess;
    Processes[FProcess].Scan(Text, Column, Line);
    if (OldColumn = Column) and (OldProcess = FProcess) then
      raise EsardException.Create('Scanner in loop with: ' + Processes[FProcess].ClassName);
  end;
end;

procedure TsardCustomScanner.Scan(const Lines: TStrings);
var
  i: Integer;
begin
  Start;
  for i := 0 to Lines.Count -1 do
  begin
    ScanLine(Lines[i] + #13, i);
  end;
  Stop;
end;

end.


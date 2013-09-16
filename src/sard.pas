unit sard;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0 (modified of http://www.gnu.org/licenses/lgpl.html)
 *            included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$M+}
{$H+}
{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}

interface

uses
  Classes, SysUtils, contnrs,
  mnStreams;

const
  OPEN_IDENTIFIER_CHARS = ['A'..'Z', 'a'..'z', '_'];
  IDENTIFIER_CHARS = OPEN_IDENTIFIER_CHARS + ['0'..'9', '.']; //for : xdebug send tag like xdebug:message
  OPERATOR_CHARS = ['+', '-', '*', '/', '^', '&'];
  SYMBOL_CHARS = ['$', '#', '@', '!', '\'];
  sSardAnsiOpen = '{?sard '; //with the space
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

  TsardFiler = class(TObject)
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

  TsardSection = (stNone, stHeader, stNormal, stString);
  TsardScanState = Integer;

  TsardProcesses = class;
  TsardCustomScanner = class;

  { TsardProcess }

  TsardProcess = class(TObject)
  private
    FProcesses: TsardProcesses;
  protected
    function IsIdentifier(vChar: AnsiChar): Boolean;
    function IsOperator(vChar: AnsiChar): Boolean;
    function CheckText(S: string; const Text:string; const Column: Integer): Boolean;
    procedure ScanTo(NextState: TsardScanState; const SubStr, Text: string; var Column: Integer; const Line: Integer); virtual;
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer); virtual; abstract;
    procedure Push(S: String); virtual;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; virtual;
    function ChooseState(const Text: string; var Column: Integer; const Line: Integer): Integer;
    procedure ChangeState(NextState: TsardScanState);
  public
    Index: Integer;
    Collected: string; //buffer
    State: TsardScanState;
    Section: TsardSection;
    constructor Create(vProcesses: TsardProcesses); virtual;
    destructor Destroy; override;
    procedure Close;
    property Processes: TsardProcesses read FProcesses;
  end;

  TsardProcessClass = class of TsardProcess;

  { TsardProcesses }

  TsardProcesses = class(TObjectList)
  private
    FScanner: TsardCustomScanner;
    function GetItem(Index: Integer): TsardProcess;
  public
    constructor Create(vScanner: TsardCustomScanner; FreeObjects: boolean = True); virtual;
    function ChooseState(const Text: string; var Column: Integer; const Line: Integer): Integer;
    function Find(const ProcessClass: TsardProcessClass): TsardProcess;
    function RegisterProcess(ProcessClass: TsardProcessClass): Integer;
    property Items[Index: Integer]: TsardProcess read GetItem; default;
    property Scanner: TsardCustomScanner read FScanner;
  end;

  { TsardScanner }

  { TsardCustomScanner }

  TsardCustomScanner = class(TsardFiler)
  private
    FState: TsardScanState;
    FProcesses: TsardProcesses;
  protected
    FDefaultState: TsardScanState; //Default process
    FOffState: TsardScanState; //Fall off into it when no one accept it
    procedure ChangeState(NextState: TsardScanState);
    procedure Push(S: String); virtual;
    property Processes: TsardProcesses read FProcesses;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ScanLine(const Text: string; const Line: Integer);
    procedure Scan(const Lines: TStrings);
  end;

  TsardScanner = class(TsardCustomScanner)
  private
  protected
  public
    constructor Create; override;
  end;

{**************************
  Common Processes
**************************}

  { TsardStartProcess }

  TsardStartProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardHeaderProcess }

  TsardHeaderProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardWhitespaceProcess }

  TsardWhitespaceProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardIdentifierProcess }

  TsardIdentifierProcess = class(TsardProcess) //none white space
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardOperatorProcess }

  TsardOperatorProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;  override;
  end;

  { TsardLineCommentProcess }

  TsardLineCommentProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardBlockCommentProcess }

  TsardBlockCommentProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardSQStringProcess }

  TsardSQStringProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

  { TsardDQStringProcess }

  TsardDQStringProcess = class(TsardProcess)
  protected
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer);  override;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; override;
  end;

var
  prcStart: Integer = 0;
  prcHeader: Integer = 0;
  prcWhiteSpace: Integer = 0;
  prcIdentifier: Integer = 0;
  prcSQString: Integer = 0;
  prcDQString: Integer = 0;
  prcBlockComment: Integer = 0;
  prcLineComment: Integer = 0;

implementation

uses
  StrUtils;

{ TsardOperatorProcess }

procedure TsardOperatorProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin

end;

function TsardOperatorProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := IsOperator(Text[Column]);
end;

{ TsardIdentifierProcess }

procedure TsardIdentifierProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
var
  c: Integer;
begin
  c := Column;
  while (Column < Length(Text)) and (Text[Column] in IDENTIFIER_CHARS) do
    Inc(Column);
  Push(MidStr(Text, c, Column - c));
  if Column< Length(Text) then
    ChooseState(Text, Column, Line);
end;

function TsardIdentifierProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := IsIdentifier(Text[Column]);//need to improve to accept unicode chars
end;

{ TsardProcess }

function TsardProcess.IsIdentifier(vChar: AnsiChar): Boolean;
begin
  Result := vChar in IDENTIFIER_CHARS;
end;

function TsardProcess.IsOperator(vChar: AnsiChar): Boolean;
begin
  Result := vChar in OPERATOR_CHARS;
end;

function TsardProcess.CheckText(S: string; const Text: string; const Column: Integer): Boolean;
begin
  Result := (Length(Text) - Column) > length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Column, Length(S))) = LowerCase(S); //caseinsensitive
end;

procedure TsardProcess.ScanTo(NextState: TsardScanState; const SubStr, Text: string; var Column: Integer; const Line: Integer);
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
    ChangeState(NextState);
  end
  else
  begin
    Column := Length(Text) + 1;
    ChangeState(State);
  end;
end;

procedure TsardProcess.Push(S: String);
begin
  Processes.Scanner.Push(S);
end;

function TsardProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;
end;

function TsardProcess.ChooseState(const Text: string; var Column: Integer; const Line: Integer): Integer;
begin
  Result := Processes.ChooseState(Text, Column, Line);
end;

procedure TsardProcess.ChangeState(NextState: TsardScanState);
begin
  //Flush;
  Processes.Scanner.ChangeState(NextState);
end;

constructor TsardProcess.Create(vProcesses: TsardProcesses);
begin
  inherited Create;
  FProcesses := vProcesses;
end;

destructor TsardProcess.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TsardProcess.Close;
begin

end;

{ TsardScanner }

constructor TsardScanner.Create;
begin
  inherited Create;

  with Processes do
  begin
    prcStart := RegisterProcess(TsardStartProcess);
    prcHeader := RegisterProcess(TsardHeaderProcess);
    prcWhiteSpace := RegisterProcess(TsardWhitespaceProcess);
    prcIdentifier := RegisterProcess(TsardIdentifierProcess);
    prcSQString := RegisterProcess(TsardSQStringProcess);
    prcDQString := RegisterProcess(TsardDQStringProcess);
    prcBlockComment := RegisterProcess(TsardBlockCommentProcess);
    prcLineComment := RegisterProcess(TsardLineCommentProcess);
  end;
end;

{ TsardDQStringProcess }

procedure TsardDQStringProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin

end;

function TsardDQStringProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('"', Text, Column);
end;

{ TsardSQStringProcess }

procedure TsardSQStringProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin

end;

function TsardSQStringProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('''', Text, Column);
end;

{ TsardBlockCommentProcess }

procedure TsardBlockCommentProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin

end;

function TsardBlockCommentProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('/*', Text, Column);
end;

{ TsardSLCommentProcess }

procedure TsardLineCommentProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin

end;

function TsardLineCommentProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := CheckText('//', Text, Column);
end;

{ TsardHeaderCommentProcess }

procedure TsardHeaderProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  ScanTo(prcWhiteSpace, '?}', Text, Column, Line)
end;

function TsardHeaderProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False; //Only when file started, or first, so not accepted
end;

{ TsardNormalCommentProcess }

procedure TsardWhitespaceProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  while (Column < Length(Text)) and (Text[Column] in sWhitespace) do
    Inc(Column);
  if Column< Length(Text) then
    ChooseState(Text, Column, Line);
end;

function TsardWhitespaceProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := Text[Column] in sWhitespace;
end;

{ TsardStartProcess }

procedure TsardStartProcess.Scan(const Text: string; var Column: Integer; const Line: Integer);
begin
  if MidStr(Text, 1, Length(sSardAnsiOpen)) = sSardAnsiOpen then
  begin
    //There is a header and it is a Ansi document
    Column := Column + Length(sSardAnsiOpen); //put the column to the first char of attributes of document
    ChangeState(prcHeader);
  end
  else
    ChangeState(prcWhiteSpace); //nop there is no header... skip to normal
end;

function TsardStartProcess.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;//Start not accept the scan, it is only when starting scan
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

function TsardProcesses.ChooseState(const Text: string; var Column: Integer; const Line: Integer): Integer;
var
  i: Integer;
begin
  Result := Scanner.FOffState;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Index <> Result) and Items[i].Accept(Text, Column, Line) then
    begin
      Result := i;
      break;
    end;
  end;
  Scanner.ChangeState(Result);
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

function TsardProcesses.RegisterProcess(ProcessClass: TsardProcessClass): Integer;
var
  aProcess: TsardProcess;
begin
  aProcess := ProcessClass.Create(Self);
  Result := Add(aProcess);
  aProcess.Index := Result;
end;

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

procedure TsardFiler.Stop;
begin
  if not FActive then
    raise EsardException.Create('File already closed');
  DoStop;
  FActive := False;
end;

constructor TsardFiler.Create(Stream: TmnWrapperStream; Owned: Boolean);
begin
  Create;
  if Stream = nil then
    raise EsardException.Create('Stream is nil');
  FStream := Stream;
  FOwned := Owned;
end;

constructor TsardFiler.Create(const FileName: string);
begin
  Create(TmnWrapperStream.Create(TFileStream.Create(FileName, fmOpenRead)));
end;

constructor TsardFiler.Create;
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

destructor TsardFiler.Destroy;
begin
{  if Active then
    Stop;}
  if FOwned then
    FStream.Free;
  FHeader.Free;
  inherited;
end;

procedure TsardFiler.DoStop;
begin
end;

procedure TsardFiler.DoStart;
begin
end;

procedure TsardFiler.Start;
begin
  if FActive then
    raise EsardException.Create('File already opened');
  FActive := True;
  DoStart;
end;

{ TsardCustomScanner }

procedure TsardCustomScanner.ChangeState(NextState: TsardScanState);
begin
  if FState <> NextState then
  begin
    FState := NextState;
  end;
end;

procedure TsardCustomScanner.Push(S: String);
begin
end;

constructor TsardCustomScanner.Create;
begin
  inherited Create;
  FProcesses := TsardProcesses.Create(Self);
end;

destructor TsardCustomScanner.Destroy;
begin
  FreeAndNil(FProcesses);
  inherited Destroy;
end;

procedure TsardCustomScanner.ScanLine(const Text: string; const Line: Integer);
var
  Column, l: Integer;
begin
  if not Active then
    raise EsardException.Create('Scanner not started');
  Column := 1; //start of pascal string is 1
  l := Length(Text);
  while (Column <= l) do
    Processes[FState].Scan(Text, Column, Line);
end;

procedure TsardCustomScanner.Scan(const Lines: TStrings);
var
  i: Integer;
begin
  Start;
  for i := 0 to Lines.Count -1 do
  begin
    ScanLine(Lines[i], i);
  end;
  Stop;
end;

end.


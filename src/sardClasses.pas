unit sardClasses;
{**
 *  This file is part of the "SARD"
 *
 * @license   Apache License Version 2.0
 *            included in this distribution,
 * @author    Zaher Dirkey <zaher at parmaja dot com>
 *}

{$IFDEF FPC}
{$mode objfpc}
{$ENDIF}
{$H+}{$M+}

{TODO:
  Check S is not empty before push
  Push with the scanner class and id
}

interface

uses
  Classes, SysUtils, Contnrs;

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

  TsardControl = (ctlDeclare, ctlAssign, ctlOpenBracket, ctlCloseBracket, ctlOpenSquare, ctlCloseSquare, ctlOpen, ctlClose, ctlLink, ctlSplit, ctlFinish, ctlComma, ctlSemicolon);
  TsardBracketKind = (brBracket, brSquare, brCurly);// and (), [], {} or maybe <>
  TsardTokinKind = (tkComment, tkIdentifier, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);
  TsardOperator = (opNone, opAdd, opMinus, opMuliple, opDivided, opSeprator);//no level until now

  TsardScannerID = type Integer;

  TsardScanners = class;
  TsardFeeder = class;
  TsardParser = class;

  TsardScannerClass = class of TsardScanner;

  { TsardScanner }

  TsardScanner = class(TObject)
  private
    FScanners: TsardScanners;
  protected
    function CheckText(S: string; const Text: string; const Column: Integer): Boolean;
    function ScanText(S: string; const Text: string; var Column: Integer): Boolean;
    procedure ScanTo(NextScanner: TsardScannerID; const SubStr, Text: string; var Column: Integer; const Line: Integer); virtual;
    procedure Scan(const Text: string; var Column: Integer; const Line: Integer); virtual; abstract;
    procedure Open(vBracket: TsardBracketKind); virtual;
    procedure Close(vBracket: TsardBracketKind); virtual;
    procedure Control(AControl: TsardControl); virtual;
    procedure Push(Token: String; TokenID: Integer); virtual;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; virtual;
    function DetectScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
    procedure SwitchScanner(AScannerID: TsardScannerID);
    procedure SelectScanner(AScannerClass: TsardScannerClass);
  public
    Index: TsardScannerID;
    Collected: string; //buffer
    Scanner: TsardScannerID;
    constructor Create(vScanners: TsardScanners); virtual;
    destructor Destroy; override;
    property Scanners: TsardScanners read FScanners;
  end;

  { TsardScanners }

  TsardScanners = class(TObjectList)
  private
    FScannerID: TsardScannerID;
    FFeeder: TsardFeeder;
    function GetItem(Index: Integer): TsardScanner;
  public
    constructor Create(vFeeder: TsardFeeder; FreeObjects: boolean = True); virtual;
    function DetectScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
    procedure SwitchScanner(NextScanner: TsardScannerID);
    procedure SelectScanner(ScannerClass: TsardScannerClass);
    function Find(const ScannerClass: TsardScannerClass): TsardScanner;
    procedure ScanLine(const Text: string; const Line: Integer);
    function RegisterScanner(ScannerClass: TsardScannerClass): TsardScannerID;
    property Items[Index: Integer]: TsardScanner read GetItem; default;
    property ScannerID: TsardScannerID read FScannerID;
    property Feeder: TsardFeeder read FFeeder;
  end;

  { TsardFeeder }

  TsardFeeder = class(TObject)
  private
    FActive: Boolean;
    FVersion: string;
    FCharset: string;
    FParser: TsardParser;//TODO use stacker
    FScanners: TsardScanners;//TODO use stacker
    procedure SetParser(AValue: TsardParser);
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;

    function CreateParser:TsardParser; virtual; abstract;
    property Scanners: TsardScanners read FScanners;
    property Parser: TsardParser read FParser write SetParser;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure ScanLine(const Text: string; const Line: Integer);
    procedure Scan(const Lines: TStrings);
    //procedure Scan(const FileName: string); overload; //TODO
    //procedure Scan(const Stream: TStream); overload; //TODO
    //procedure Scan(const Stream: IStream); overload; //TODO

    procedure Start;
    procedure Stop;
    property Active: Boolean read FActive write FActive;
    property Version: string read FVersion write FVersion;
    property Charset: string read FCharset write FCharset;
  end;

  { TsardParser }

  TsardParser = class(TObject)
  protected
    procedure Open(vBracket: TsardBracketKind); virtual; abstract;
    procedure Push(Token: String; TokenID: Integer); virtual; abstract;
    procedure Close(vBracket: TsardBracketKind); virtual; abstract;
    procedure Control(AControl: TsardControl); virtual;
  end;

  TsardStack = class;

  TsardStackItem = class(TObject)
  public
    AnObject: TObject;
    Owner: TsardStack;
    Prior: TsardStackItem;
  end;

  { TsardStack }

  TsardStack = class(TObject)
  private
    FCount: Integer;
    FCurrent: TsardStackItem;
    function GetCurrent: TObject;
    procedure SetCurrent(const AValue: TObject);
  public
    function IsEmpty: Boolean;
    procedure Push(vObject: TObject);
    function Pop: TObject;
    procedure Delete;
    function Peek: TObject;
    property Current: TObject read GetCurrent write SetCurrent;
    property Count: Integer read FCount;
  end;

implementation

uses
  StrUtils;

{ TsardParser }

procedure TsardParser.Control(AControl: TsardControl);
begin
end;

procedure TsardStack.Delete;
var
  aNode: TsardStackItem;
  aObject: TObject;
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  aObject := FCurrent.AnObject;
  aNode := FCurrent;
  FCurrent := aNode.Prior;
  Dec(FCount);
  aNode.Free;
  aObject.Free;
end;

function TsardStack.GetCurrent: TObject;
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  Result := FCurrent.AnObject;
end;

function TsardStack.IsEmpty: Boolean;
begin
  Result := FCurrent = nil;
end;

function TsardStack.Peek: TObject;
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  Result := FCurrent.AnObject;
end;

function TsardStack.Pop: TObject;
var
  aNode: TsardStackItem;
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  Result := FCurrent.AnObject;
  aNode := FCurrent;
  FCurrent := aNode.Prior;
  aNode.Free;
  Dec(FCount);
end;

procedure TsardStack.Push(vObject: TObject);
var
  aNode: TsardStackItem;
begin
  aNode := TsardStackItem.Create;
  aNode.AnObject := vObject;
  aNode.Prior := FCurrent;
  aNode.Owner := Self;
  FCurrent := aNode;
  Inc(FCount);
end;

procedure TsardStack.SetCurrent(const AValue: TObject);
begin
  if FCurrent = nil then
    raise EsardException.Create('Stack is empty');
  FCurrent.AnObject := AValue;
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

{ TsardScanner }

function TsardScanner.CheckText(S: string; const Text: string; const Column: Integer): Boolean;
begin
  Result := (Length(Text) - Column) >= length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Column, Length(S))) = LowerCase(S); //caseinsensitive
end;

function TsardScanner.ScanText(S: string; const Text: string; var Column: Integer): Boolean;
begin
  Result := (Length(Text) - Column) >= length(S);
  if Result then
    Result := LowerCase(MidStr(Text, Column, Length(S))) = LowerCase(S); //caseinsensitive
  if Result then
    Column := Column + Length(S);
end;

procedure TsardScanner.ScanTo(NextScanner: TsardScannerID; const SubStr, Text: string; var Column: Integer; const Line: Integer);
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
    SwitchScanner(NextScanner);
  end
  else
  begin
    Column := Length(Text) + 1;
    SwitchScanner(Scanner);
  end;
end;

procedure TsardScanner.Open(vBracket: TsardBracketKind);
begin
  Scanners.Feeder.Parser.Open(vBracket);
end;

procedure TsardScanner.Close(vBracket: TsardBracketKind);
begin
  Scanners.Feeder.Parser.Close(vBracket);
end;

procedure TsardScanner.Control(AControl: TsardControl);
begin
  Scanners.Feeder.Parser.Control(AControl);
end;

procedure TsardScanner.Push(Token: String; TokenID: Integer);
begin
  Scanners.Feeder.Parser.Push(Token, TokenID);
end;

function TsardScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;
end;

function TsardScanner.DetectScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
begin
  Result := Scanners.DetectScanner(Text, Column, Line);
end;

procedure TsardScanner.SwitchScanner(AScannerID: TsardScannerID);
begin
  Scanners.SwitchScanner(AScannerID);
end;

procedure TsardScanner.SelectScanner(AScannerClass: TsardScannerClass);
begin
  Scanners.SelectScanner(AScannerClass);
end;

constructor TsardScanner.Create(vScanners: TsardScanners);
begin
  inherited Create;
  FScanners := vScanners;
end;

destructor TsardScanner.Destroy;
begin
  inherited Destroy;
end;

{ TsardScanners }

function TsardScanners.GetItem(Index: Integer): TsardScanner;
begin
  Result := inherited Items[Index] as TsardScanner;
end;

constructor TsardScanners.Create(vFeeder: TsardFeeder; FreeObjects: boolean);
begin
  FFeeder := vFeeder;
  inherited Create(FreeObjects);
end;

function TsardScanners.DetectScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Index <> Result) and Items[i].Accept(Text, Column, Line) then
    begin
      Result := i;
//      WriteLn('Accept: '+Items[i].ClassName);
      break;
    end;
  end;
  if Result < 0 then
    raise EsardException.Create('Scanner not found:' + Text[Column]);
  SwitchScanner(Result);
end;

function TsardScanners.Find(const ScannerClass: TsardScannerClass): TsardScanner;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Count - 1 do
  begin
    if ScannerClass = Items[i].ClassType then
    begin
      Result := Items[i];
      break;
    end;
  end;
end;

function TsardScanners.RegisterScanner(ScannerClass: TsardScannerClass): TsardScannerID;
var
  aScanner: TsardScanner;
begin
  aScanner := ScannerClass.Create(Self);
  Result := Add(aScanner);
  aScanner.Index := Result;
end;

procedure TsardFeeder.Stop;
begin
  if not FActive then
    raise EsardException.Create('File already closed');
  DoStop;
  FActive := False;
end;


procedure TsardFeeder.Start;
begin
  if FActive then
    raise EsardException.Create('File already opened');
  FActive := True;
  DoStart;
end;

procedure TsardFeeder.SetParser(AValue: TsardParser);
begin
  if FParser = AValue then Exit;
  FParser := AValue;
end;

procedure TsardFeeder.DoStart;
begin
end;

procedure TsardFeeder.DoStop;
begin
end;

procedure TsardScanners.SwitchScanner(NextScanner: TsardScannerID);
begin
  if FScannerID <> NextScanner then
  begin
    FScannerID := NextScanner;
  end;
end;

procedure TsardScanners.SelectScanner(ScannerClass: TsardScannerClass);
var
  aScanner: TsardScanner;
begin
  aScanner := Find(ScannerClass);
  if aScanner = nil then
    raise EsardException.Create('Scanner not found');
  SwitchScanner(aScanner.Index);
end;

constructor TsardFeeder.Create;
begin
  inherited Create;
  FVersion := '1.0';
  {$ifdef FPC}
  FCharset := 'utf-8';
  {$else}
  FCharset := 'iso-8859-1';
  {$endif}
  FScanners := TsardScanners.Create(Self);
  Parser := CreateParser;
end;

destructor TsardFeeder.Destroy;
begin
  FreeAndNil(FScanners);
  inherited Destroy;
end;

procedure TsardFeeder.ScanLine(const Text: string; const Line: Integer);
begin
  if not Active then
    raise EsardException.Create('Feeder not started');
  Scanners.ScanLine(Text, Line);
end;

procedure TsardScanners.ScanLine(const Text: string; const Line: Integer);
var
  Column, OldColumn: Integer;
  OldScanner: TsardScannerID;
  l: Integer;
begin
  Column := 1; //start of pascal string is 1
  l := Length(Text);
  while (Column <= l) do
  begin
    OldColumn := Column;
    OldScanner := FScannerID;

    Items[ScannerID].Scan(Text, Column, Line);
    if Column <= Length(Text) then
      DetectScanner(Text, Column, Line);

    if (OldColumn = Column) and (OldScanner = FScannerID) then
      raise EsardException.Create('Feeder in loop with: ' + Items[FScannerID].ClassName);
  end;
end;

procedure TsardFeeder.Scan(const Lines: TStrings);
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

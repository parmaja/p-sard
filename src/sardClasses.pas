unit sardClasses;
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

  sardBracketKind = (brParenthesis, brSquare, brCurly);// and (), [], {} or maybe <>
  sardTokinKind = (tkComment, tkIdentifier, tkNumber, tkSpace, tkString, tkSymbol, tkUnknown);

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
    procedure Open(vBracket: sardBracketKind); virtual;
    procedure Close(vBracket: sardBracketKind); virtual;
    procedure Terminate; virtual;
    procedure Push(Token: String; TokenID: Integer); virtual;
    function Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean; virtual;
    function ChooseScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
    procedure SwitchScanner(NextScanner: TsardScannerID);
    procedure SelectScanner(ScannerClass: TsardScannerClass);
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
    FFeeder: TsardFeeder;
    function GetItem(Index: Integer): TsardScanner;
  public
    constructor Create(vFeeder: TsardFeeder; FreeObjects: boolean = True); virtual;
    function ChooseScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
    function Find(const ScannerClass: TsardScannerClass): TsardScanner;
    function RegisterScanner(ScannerClass: TsardScannerClass): TsardScannerID;
    property Items[Index: Integer]: TsardScanner read GetItem; default;
    property Feeder: TsardFeeder read FFeeder;
  end;

  { TsardFeeder }

  TsardFeeder = class(TObject)
  private
    FActive: Boolean;
    FVersion: string;
    FHeader: TStringList;
    FCharset: string;
    FStandalone: Boolean;
    FParser: TsardParser;
    FScanner: TsardScannerID;
    FScanners: TsardScanners;
    procedure SetParser(AValue: TsardParser);
  protected
    FDefaultScanner: TsardScannerID; //Default Scanner
    FOffScanner: TsardScannerID; //Fall off into it when no one accept it
    procedure DoStart; virtual;
    procedure DoStop; virtual;

    procedure SwitchScanner(NextScanner: TsardScannerID);
    procedure SelectScanner(ScannerClass: TsardScannerClass);

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
    property Header: TStringList read FHeader write FHeader;
    property Standalone: Boolean read FStandalone write FStandalone;
    property Version: string read FVersion write FVersion;
    property Charset: string read FCharset write FCharset;
  end;

  { TsardParser }

  TsardParser = class(TObject)
  protected
    procedure Open(vBracket: sardBracketKind); virtual; abstract;
    procedure Close(vBracket: sardBracketKind); virtual; abstract;
    procedure Terminate; virtual; abstract;
    procedure Push(Token: String; TokenID: Integer); virtual; abstract;
  end;

  { TsardObject }

  TsardObject = class(TObject)
  end;

  { TsardBlock }

  TsardBlock = class(TsardObject)
  private
    FName: string;
    FParent: TsardBlock;
    procedure SetName(AValue: string);
  public
    constructor Create(AParent: TsardBlock); virtual;
    property Parent: TsardBlock read FParent;
    property Name: string read FName write SetName;
  end;

  { TsardBlocks }

  TsardBlocks = class(TObjectList)
  private
    function GetItem(Index: Integer): TsardBlock;
  public
    property Items[Index: Integer]: TsardBlock read GetItem; default;
  end;

  TsardElements = class;

  TsardElement = class(TsardBlock)
  private
    FElements: TsardElements;
    FLinkOperator: string;
  public
    constructor Create(AParent: TsardBlock); override;
    destructor Destroy; override;
    property Elements: TsardElements read FElements;
    property LinkOperator: string read FLinkOperator;
  end;

  { TsardElements }

  TsardElements = class(TsardBlocks)
  private
    function GetItem(Index: Integer): TsardElement;
  public
    property Items[Index: Integer]: TsardElement read GetItem; default;
  end;

implementation

uses
  StrUtils;

{ TsardElements }

function TsardElements.GetItem(Index: Integer): TsardElement;
begin
  Result := inherited Items[Index] as TsardElement;
end;

{ TsardElement }

constructor TsardElement.Create(AParent: TsardBlock);
begin
  inherited Create(AParent);
  FElements := TsardElements.Create;
end;

destructor TsardElement.Destroy;
begin
  FreeAndNil(FElements);
  inherited Destroy;
end;

{ TsardBlocks }

function TsardBlocks.GetItem(Index: Integer): TsardBlock;
begin
  Result := inherited Items[Index] as TsardBlock;
end;

{ TsardBlock }

procedure TsardBlock.SetName(AValue: string);
begin
  if FName <> AValue then
  begin
    //CheckUniqe; TODO
    FName := AValue;
  end;
end;

constructor TsardBlock.Create(AParent: TsardBlock);
begin
  inherited Create;
  FParent := AParent;
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

procedure TsardScanner.Open(vBracket: sardBracketKind);
begin
  Scanners.Feeder.Parser.Open(vBracket);
end;

procedure TsardScanner.Close(vBracket: sardBracketKind);
begin
  Scanners.Feeder.Parser.Close(vBracket);
end;

procedure TsardScanner.Terminate;
begin
  Scanners.Feeder.Parser.Terminate;
end;

procedure TsardScanner.Push(Token: String; TokenID: Integer);
begin
  Scanners.Feeder.Parser.Push(Token, TokenID);
end;

function TsardScanner.Accept(const Text: string; var Column: Integer; const Line: Integer): Boolean;
begin
  Result := False;
end;

function TsardScanner.ChooseScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
begin
  Result := Scanners.ChooseScanner(Text, Column, Line);
end;

procedure TsardScanner.SwitchScanner(NextScanner: TsardScannerID);
begin
  Scanners.Feeder.SwitchScanner(NextScanner);
end;

procedure TsardScanner.SelectScanner(ScannerClass: TsardScannerClass);
begin
  Scanners.Feeder.SelectScanner(ScannerClass);
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

function TsardScanners.ChooseScanner(const Text: string; var Column: Integer; const Line: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;//Feeder.FOffScanner;
  for i := 0 to Count - 1 do
  begin
    if (Items[i].Index <> Result) and Items[i].Accept(Text, Column, Line) then
    begin
      Result := i;
      break;
    end;
  end;
  if Result < 0 then
    raise EsardException.Create('Scanner not found:' + Text[Column]);
  Feeder.SwitchScanner(Result);
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

{ TsardFeeder }

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

procedure TsardFeeder.SwitchScanner(NextScanner: TsardScannerID);
begin
  if FScanner <> NextScanner then
  begin
    if NextScanner = 0 then
      FScanner := FScanner;
    FScanner := NextScanner;
  end;
end;

procedure TsardFeeder.SelectScanner(ScannerClass: TsardScannerClass);
var
  aScanner: TsardScanner;
begin
  aScanner := Scanners.Find(ScannerClass);
  if aScanner = nil then
    raise EsardException.Create('Scanner not found');
  SwitchScanner(aScanner.Index);
end;

constructor TsardFeeder.Create;
begin
  inherited Create;
  FHeader := TStringList.Create;
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
  FHeader.Free;
  FreeAndNil(FScanners);
  inherited Destroy;
end;

procedure TsardFeeder.ScanLine(const Text: string; const Line: Integer);
var
  Column, OldColumn: Integer;
  OldScanner: TsardScannerID;
  l: Integer;
begin
  if not Active then
    raise EsardException.Create('Feeder not started');
  Column := 1; //start of pascal string is 1
  l := Length(Text);
  while (Column <= l) do
  begin
    OldColumn := Column;
    OldScanner := FScanner;
    Scanners[FScanner].Scan(Text, Column, Line);
    if (OldColumn = Column) and (OldScanner = FScanner) then
      raise EsardException.Create('Feeder in loop with: ' + Scanners[FScanner].ClassName);
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


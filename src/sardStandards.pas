unit sardStandards;
{**
 *  This file is part of the "SARD"
 *
 * @license   The MIT License (MIT)
 *            Included in this distribution
 * @author    Zaher Dirkey
 *}

{$IFDEF FPC}
{$WARN 5024 off : Parameter "$1" not used}
{$mode delphi}
{$ENDIF}
{$H+}{$M+}

interface

uses
  Classes, SysUtils,
  mnUtils,
  mnClasses, sardClasses, sardParsers;


const
  sEOL = [#0, #13, #10];
  sEscape = '\';

type
  { TMultiLine_Tokenizer }

  TMultiLine_Tokenizer = class abstract(TTokenizer)
  protected
    OpenSymbol: string;
    CloseSymbol: string;
    procedure Appended; virtual; abstract;
    procedure Append(const Text: string); virtual; abstract;

    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  end;

  { TBufferedMultiLine_Tokenizer }

  TBufferedMultiLine_Tokenizer = class(TMultiLine_Tokenizer)
  private
    Buffer: string;
  protected
    procedure InternalSetToken(const Text: string); virtual; abstract;
    procedure Append(const Text: string); override;
    procedure Appended; override;
  end;

  { TString_Tokenizer }

  TString_Tokenizer = class abstract(TBufferedMultiLine_Tokenizer)
  public
    procedure InternalSetToken(const Text: string); override;
  end;

  { TWhitespace_Tokenizer }

  TWhitespace_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  end;

  { TIdentifier_Tokenizer }

  TIdentifier_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  end;

  { TNumber_Tokenizer }

  TNumber_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  end;

  { TControl_Tokenizer }

  TControl_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  end;

  { TLineComment_Tokenizer }

  TLineComment_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  end;

  { TBlockComment_Tokenizer }

  TBlockComment_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  end;

  //Comment object {* *}

  { TComment_Tokenizer }

  TComment_Tokenizer = class(TBufferedMultiLine_Tokenizer)
  public
    constructor Create; override;
    procedure InternalSetToken(const Text: string); override;
  end;

  {* Single Quote String *}

  { TSQString_Tokenizer }

  TSQString_Tokenizer = class(TString_Tokenizer)
  public
    constructor Create; override;
  end;

  {* Double Quote String *}

  { DQString_Tokenizer }

  TDQString_Tokenizer = class(TString_Tokenizer)
  public
    constructor Create; override;
  end;

  TStringState = (ssNone, ssEscape);

  { SL_String_Tokenizer }

  TSL_String_Tokenizer = class(TTokenizer)
  public
    Buffer: String;
    State: TStringState;
    QuoteChar: Char;
    EscapeChar: Char;
    constructor Create; override;

    procedure Appended; virtual;
    procedure Append(const Text: string); virtual;

    function Accept(const Text: string; Column: Integer): Boolean; override;
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
  end;

  { TEscape_Tokenizer }

  TEscape_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; Column: Integer): Boolean; override;
  public
  end;


implementation

{ TMultiLine_Tokenizer }

procedure TMultiLine_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  if not Resume then
  begin
    Column := Column + Length(openSymbol);
  end;

  while (IndexInStr(Column, Text)) do
  begin
    if ScanCompare(CloseSymbol, Text, Column) then
    begin
      if (not Lexer.TrimToken) then
        Column := Column + Length(CloseSymbol);
      Append(SliceText(Text, Started, Column));
      if (Lexer.TrimToken) then
          Column := Column + Length(CloseSymbol);
      Appended;
      Resume := False;
      exit;
    end;
    Inc(Column);
  end;
  Append(SliceText(Text, Started, Column));
  Resume := True;
end;

function TMultiLine_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := ScanString(openSymbol, text, column);
end;

{ TBufferedMultiLine_Tokenizer }

procedure TBufferedMultiLine_Tokenizer.Append(const Text: string);
begin
  Buffer := Buffer + Text;
end;

procedure TBufferedMultiLine_Tokenizer.Appended;
begin
  InternalSetToken(Buffer);
  Buffer := '';
end;

procedure TString_Tokenizer.InternalSetToken(const Text: string);
begin
  SetToken(Token(ctlToken, typeString, Text));
end;

{ TEscape_Tokenizer }

procedure TEscape_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column); //not need first char, it is not pass from isIdentifier
  //print("Hello "\n"World"); //but add " to the world
  while (IndexInStr(Column, text) and (Lexer.isIdentifier(Text[Column], False))) do
    Inc(Column);
  SetToken(Token(ctlToken, typeEscape, SliceText(Text, Started, Column)));
  Resume := False;
end;

function TEscape_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := Text[Column] = sEscape;
end;

{ TDQString_Tokenizer }

constructor TDQString_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '"';
  CloseSymbol := '"';
end;

{ TSQString_Tokenizer }

constructor TSQString_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '''';
  CloseSymbol := '''';
end;

{ TComment_Tokenizer }

constructor TComment_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '{*';
  CloseSymbol := '*}';
end;

procedure TComment_Tokenizer.InternalSetToken(const Text: string);
begin
  SetToken(Token(ctlToken, typeComment, text));
end;

{ TBlockComment_Tokenizer }

procedure TBlockComment_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  while IndexInStr(Column, text) do
  begin
      if (ScanString('*/', Text, Column)) then
      begin
          Resume := False;
          exit;
      end;
      Inc(Column);
  end;
  Inc(Column);;//Eat the second chat //not sure
  Resume := True;
end;

function TBlockComment_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := ScanString('/*', Text, Column);
end;

{ TLineComment_Tokenizer }

procedure TLineComment_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while IndexInStr(Column, Text) and (not Lexer.IsEOL(Text[Column])) do
    inc(Column);
  inc(Column);//Eat the EOF char
  Resume := False;
end;

function TLineComment_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := ScanString('//', Text, Column);
end;

{ TControl_Tokenizer }

function TControl_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.IsControl(Text[Column]);
end;

procedure TControl_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
var
  AControl: TSardControl;
begin
  AControl := Lexer.Controls.Scan(Text, Column);
  if AControl <> nil then
    Column := Column + Length(AControl.Name)
  else
    RaiseError('Unkown control started with ' + Text[Started]);
  Lexer.Parser.SetControl(AControl);
  Resume := False;
end;

{ TNumber_Tokenizer }

procedure TNumber_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsNumber(Text[Column], False))) do
    inc(Column);
  SetToken(Token(ctlToken, typeNumber, SliceText(Text, Started, Column)));
  Resume := false;
end;

function TNumber_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.IsNumber(Text[Column], True);
end;

{ TIdentifier_tokenizer }

procedure TIdentifier_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsIdentifier(Text[Column], False))) do
    inc(Column);
  SetToken(Token(ctlToken, typeIdentifier, SliceText(Text, Started, Column)));
  Resume := false;
end;

function TIdentifier_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.IsIdentifier(Text[Column], True);
end;

{ TWhitespace_Tokenizer }

procedure TWhitespace_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsWhiteSpace(Text[Column]))) do
    inc(Column);
  Resume := false;
end;

function TWhitespace_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := Lexer.isWhiteSpace(Text[Column]);
end;

{ TSL_String_Tokenizer }

function TSL_String_Tokenizer.Accept(const Text: string; Column: Integer): Boolean;
begin
  Result := Text[Column] = QuoteChar;
end;

procedure TSL_String_Tokenizer.Append(const Text: string);
begin
  Buffer := Buffer + Text;
end;

procedure TSL_String_Tokenizer.Appended;
begin
  SetToken(Token(ctlToken, typeString, Buffer));
  Buffer := '';
end;

constructor TSL_String_Tokenizer.Create;
begin
  inherited;
  QuoteChar := '"';
  EscapeChar := '\';
end;

procedure TSL_String_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  if not Resume then //we already take it from accept
  begin
    Column := Column + 1;
    Started := Started + 1;
  end;

  while (IndexInStr(Column, Text)) do
  begin
    if State = ssEscape then
    begin
      case Text[Column] of
        'n': Append(#13);
        'r': Append(#10);
        '0': Append(#0);
        else
          Append(Text[Column]);
      end;
      Started := Column + 1;
      State := ssNone;
    end
    else if (Text[Column] = EscapeChar) then
    begin
      Append(SliceText(Text, Started, Column));
      Started := Column;
      State := ssEscape;
    end
    else if Text[Column] = QuoteChar then
    begin
      Append(SliceText(Text, Started, Column));
      Appended;
      Inc(Column);
      Resume := False;
      exit;
    end
    else if Lexer.IsEOL(Text[Column]) then
    begin
      RaiseError('String not end!');
    end;
    Inc(Column);
  end;
  Resume := True;
end;

end.

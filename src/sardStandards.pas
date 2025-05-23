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

type
  { TEnclosed_Tokenizer }

  TEnclosed_Tokenizer = class abstract(TTokenizer)
  protected
    OpenSymbol: string;
    CloseSymbol: string;
    procedure Appended; virtual; abstract;
    procedure Append(const Text: string); virtual; abstract;

    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TBufferedEnclosed_Tokenizer }

  TBufferedEnclosed_Tokenizer = class(TEnclosed_Tokenizer)
  private
    Buffer: string;
  protected
    procedure InternalSetToken(const Text: string); virtual; abstract;
    procedure Append(const Text: string); override;
    procedure Appended; override;
  end;

  { TML_String_Tokenizer }

  TML_String_Tokenizer = class abstract(TBufferedEnclosed_Tokenizer)
  public
    procedure InternalSetToken(const Text: string); override;
  end;

  { TWhitespace_Tokenizer }

  TEOL_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TWhitespace_Tokenizer }

  TWhitespace_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TIdentifier_Tokenizer }

  TIdentifier_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  { TNumber_Tokenizer }

  TNumber_Tokenizer = class(TTokenizer)
  protected
    function Accept(const Text: string; var Column: Integer): Boolean; override;
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
  end;

  { TSL_Comment_Tokenizer }

  TSL_Comment_Tokenizer = class(TTokenizer)
  protected
    function Accept(const Text: string; var Column: Integer): Boolean; override;
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
  end;

  { TML_Comment_Tokenizer }

  TML_Comment_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  end;

  //Comment object {* *}

  { TSardComment_Tokenizer }

  TSardComment_Tokenizer = class(TBufferedEnclosed_Tokenizer)
  public
    constructor Create; override;
    procedure InternalSetToken(const Text: string); override;
  end;

  {* Single Quote String *}

  { TSQString_Tokenizer }

  TML_SQString_Tokenizer = class(TML_String_Tokenizer)
  public
    constructor Create; override;
  end;

  {* Double Quote String *}

  { DQString_Tokenizer }

  TML_DQString_Tokenizer = class(TML_String_Tokenizer)
  public
    constructor Create; override;
  end;

  TStringState = (ssNone, ssEscape);

  { SL_String_Tokenizer }

  TSL_DQ_String_Tokenizer = class(TTokenizer)
  public
    StringBuffer: String;
    State: TStringState;
    QuoteChar: Char;
    EscapeChar: Char;
    constructor Create; override;

    procedure Appended; virtual;
    procedure Append(const Text: string); virtual;

    function Accept(const Text: string; var Column: Integer): Boolean; override;
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
  end;

  { TOut_Escape_Tokenizer }

  TOut_Escape_Tokenizer = class(TTokenizer)
  protected
    procedure Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean); override;
    function Accept(const Text: string; var Column: Integer): Boolean; override;
  public
  end;

implementation

{ TEnclosed_Tokenizer }

procedure TEnclosed_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
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

function TEnclosed_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanString(openSymbol, text, column);
end;

{ TBufferedEnclosed_Tokenizer }

procedure TBufferedEnclosed_Tokenizer.Append(const Text: string);
begin
  Buffer := Buffer + Text;
end;

procedure TBufferedEnclosed_Tokenizer.Appended;
begin
  InternalSetToken(Buffer);
  Buffer := '';
end;

procedure TML_String_Tokenizer.InternalSetToken(const Text: string);
begin
  SetToken(Token(ctlToken, typeString, Text));
end;

{ TOut_Escape_Tokenizer }

procedure TOut_Escape_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column); //not need first char, it is not pass from isIdentifier
  //print("Hello "\n"World"); //but add " to the world
  while (IndexInStr(Column, text) and (Lexer.isIdentifier(Text[Column], False))) do
    Inc(Column);
  SetToken(Token(ctlToken, typeEscape, SliceText(Text, Started, Column)));
  Resume := False;
end;

function TOut_Escape_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Text[Column] = sEscape;
  if Result then
    Inc(Column);
end;

{ TML_DQString_Tokenizer }

constructor TML_DQString_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '"';
  CloseSymbol := '"';
end;

{ TML_SQString_Tokenizer }

constructor TML_SQString_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '''';
  CloseSymbol := '''';
end;

{ TSardComment_Tokenizer }

constructor TSardComment_Tokenizer.Create;
begin
  inherited;
  OpenSymbol := '{*';
  CloseSymbol := '*}';
end;

procedure TSardComment_Tokenizer.InternalSetToken(const Text: string);
begin
  SetToken(Token(ctlToken, typeComment, text));
end;
{ TML_Comment_Tokenizer }

procedure TML_Comment_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
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

function TML_Comment_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanString('/*', Text, Column);
end;

{ TSL_Comment_Tokenizer }

procedure TSL_Comment_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  while IndexInStr(Column, Text) and (not Lexer.IsEOL(Text[Column])) do
    inc(Column);
  inc(Column);//Eat the EOF char
  Resume := False;
end;

function TSL_Comment_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := ScanString('//', Text, Column);
end;

{ TNumber_Tokenizer }

function TNumber_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexer.IsNumber(Text[Column], True);
end;

procedure TNumber_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsNumber(Text[Column], False))) do
    inc(Column);
  SetToken(Token(ctlToken, typeNumber, SliceText(Text, Started, Column)));
  Resume := false;
end;

{ TIdentifier_tokenizer }

function TIdentifier_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexer.IsIdentifier(Text[Column], True);
end;

procedure TIdentifier_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsIdentifier(Text[Column], False))) do
    inc(Column);
  SetToken(Token(ctlToken, typeIdentifier, SliceText(Text, Started, Column)));
  Resume := false;
end;

{ TEOL_Tokenizer }

function TEOL_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexer.IsEOL(Text[Column]);
end;

procedure TEOL_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  Resume := False;
end;

{ TWhitespace_Tokenizer }

function TWhitespace_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Lexer.isWhiteSpace(Text[Column]);
end;

procedure TWhitespace_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  Inc(Column);
  while (IndexInStr(Column, Text) and (Lexer.IsWhiteSpace(Text[Column]))) do
    inc(Column);
  Resume := false;
end;

{ TSL_DQ_String_Tokenizer }

function TSL_DQ_String_Tokenizer.Accept(const Text: string; var Column: Integer): Boolean;
begin
  Result := Text[Column] = QuoteChar;
  if Result then
    Inc(Column);
end;

procedure TSL_DQ_String_Tokenizer.Append(const Text: string);
begin
  StringBuffer := StringBuffer + Text;
end;

procedure TSL_DQ_String_Tokenizer.Appended;
begin
  SetToken(Token(ctlToken, typeString, StringBuffer));
  StringBuffer := '';
end;

constructor TSL_DQ_String_Tokenizer.Create;
begin
  inherited;
  QuoteChar := '"';
  EscapeChar := '\';
end;

procedure TSL_DQ_String_Tokenizer.Scan(const Text: string; Started: Integer; var Column: Integer; var Resume: Boolean);
begin
  if not Resume then //we already take it from accept
  begin
    Column := Column + 1;
    Started := Started + 1;
  end;

  while (IndexInStr(Column, Text)) and not Lexer.IsEOL(Text[Column]) do
  begin
    if State = ssEscape then
    begin
      case Text[Column] of
        'b': Append(#8);
        't': Append(#9);
        'n': Append(#10);
        'r': Append(#13);
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
    end;
    Inc(Column);
  end;
  Resume := False;
  RaiseError('String not end!');
end;

end.

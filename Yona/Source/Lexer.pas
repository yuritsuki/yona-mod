UNIT Lexer;
{
DESCRIPTION:  Lexems reader for modified Oberon grammatics
AUTHOR:       Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  INTERFACE  (***)
USES
  SysUtils,
  Utils, TextScan, Lang, LexerLng;


CONST
  (*
    anychar   = #0..#255
    char      = anychar - #10 - #13
    digit     = '0'..'9'
    hexdigit  = digit | 'A'..'F'
    letter    = 'a'..'z' | 'A'..'Z' | "_"
  *)

  (* TLexType *)
  LEX_NO_ITEM               = 0;  // No more lexems to read
  LEX_ERROR                 = 1;  // Invalid lexem

  LEX_STRING                = 2;  // '"' {anychar} '"' | "'" {anychar} "'" | digit {hexdigit} "X"
  LEX_COMMENT               = 3;  // "(*" (comment | {anychar}) "*)"
  LEX_INTEGER               = 4;  // digit ({digit} | {hexdigit} "H")
  LEX_REAL                  = 5;  // digit {digit} "." {digit}
  LEX_IDENT                 = 6;  // letter {letter | digit}

  LEX_PLUS                  = 7;  // "+"
  LEX_MINUS                 = 8;  // "-"
  LEX_ASTERISK              = 9;  // "*"
  LEX_SLASH                 = 10; // "/"
  LEX_TILDE                 = 11; // '~'
  LEX_AMPERSAND             = 12; // '&'
  LEX_DOT                   = 13; // "."
  LEX_COMMA                 = 14; // ","
  LEX_SEMICOLON             = 15; // ";"
  LEX_VERTBAR               = 16; // "|"
  LEX_BECOMES               = 17; // ":="
  LEX_CARET                 = 18; // "^"
  LEX_EQUALS                = 19; // "="
  LEX_NOT_EQUALS            = 20; // "#"
  LEX_GREATER               = 21; // ">"
  LEX_LESS                  = 22; // "<"
  LEX_GREATER_EQUALS        = 23; // ">="
  LEX_LESS_EQUALS           = 24; // "<="
  LEX_DOUBLEDOT             = 25; // ".."
  LEX_COLON                 = 26; // ":"
  LEX_LEFT_PARANTHESIS      = 27; // "("
  LEX_RIGHT_PARANTHESIS     = 28; // ")"
  LEX_LEFT_SQUARE_BRACKET   = 29; // "["
  LEX_RIGHT_SQUARE_BRACKET  = 30; // "]"
  LEX_LEFT_CURLY_BRACE      = 31; // "{"
  LEX_RIGHT_CURLY_BRACE     = 32; // "}"

  LEX_LAST                  = 33;

TYPE
  TLexType  = INTEGER;

  TLexem  = RECORD
    LexType:  TLexType;
    Token:    STRING;
    Pos:      INTEGER;
    Line:     INTEGER;
    LinePos:  INTEGER;
    Error:    STRING;
    StrVal:   STRING;
    IntVal:   INTEGER;
    RealVal:  SINGLE;
  END; // .RECORD TLexem


(*  Lexem.Error, Lexem.Token, Lexem.Pos and Lexem.Error are set on error  *)
FUNCTION  ReadLexem ({i} VAR Lexem: TLexem; Scanner: TextScan.TTextScanner): BOOLEAN;


(***) IMPLEMENTATION (***)


VAR
{O} Lng:            LexerLng.PLangStringsArr;
    LexTable:       ARRAY [0..ORD(HIGH(CHAR))] OF TLexType;
    OneCharStrings: ARRAY [0..ORD(HIGH(CHAR))] OF STRING;


FUNCTION ReadLexem ({i} VAR Lexem: TLexem; Scanner: TextScan.TTextScanner(*; IdentCharset: Utils.TCharSet*)): BOOLEAN;
CONST
  BLANKS    = [#0..#32];
  DIGITS    = ['0'..'9'];
  HEXDIGITS = DIGITS + ['A'..'F'];
  LETTERS   = ['a'..'z', 'A'..'Z', '_'];
  IDENTS    = LETTERS + DIGITS;

  QUOTE_LEN         = LENGTH('"');
  QUOTES_LEN        = QUOTE_LEN * 2;
  CHAR_TOKEN_LEN    = QUOTES_LEN + SIZEOF(AnsiChar);
  COMMENT_TAG_LEN   = LENGTH('(*');
  COMMENT_TAGS_LEN  = COMMENT_TAG_LEN * 2;

VAR
  c:              CHAR;
  FormatSettings: SysUtils.TFormatSettings;

  FUNCTION ParseIdent: BOOLEAN;
  BEGIN
    Lexem.LexType :=  LEX_IDENT;
    Scanner.ReadToken(IDENTS, Lexem.Token);
    Lexem.StrVal  :=  Lexem.Token;
    RESULT        :=  TRUE;
  END; // .FUNCTION ParseIdent

  FUNCTION ParseNumber: BOOLEAN;
    FUNCTION ParseRealNumber: BOOLEAN;
    BEGIN
      Scanner.GotoNextChar;
      Scanner.SkipCharset(DIGITS);
      Lexem.Token :=  Scanner.GetSubstrAtPos(Lexem.Pos, Scanner.Pos - Lexem.Pos);
      RESULT      :=  SysUtils.TryStrToFloat(Lexem.Token, Lexem.RealVal, FormatSettings);

      IF RESULT THEN BEGIN
        Lexem.LexType :=  LEX_REAL;
      END // .IF
      ELSE BEGIN
        Lexem.Error :=  Lng[NUMBER_TOO_LARGE];
      END; // .ELSE
    END; // .FUNCTION ParseRealNumber

    FUNCTION ParseHexNumber: BOOLEAN;
    BEGIN
      Scanner.SkipCharset(HEXDIGITS);
      RESULT      :=  Scanner.GetCurrChar(c) AND (c IN ['H', 'X']);
      Scanner.GotoNextChar;
      Lexem.Token :=  Scanner.GetSubstrAtPos(Lexem.Pos, Scanner.Pos - Lexem.Pos);

      IF RESULT THEN BEGIN
        RESULT  :=  SysUtils.TryStrToInt
        (
          '$' + COPY(Lexem.Token, 1, LENGTH(Lexem.Token) - 1), Lexem.IntVal
        );

        IF RESULT AND (c = 'X') THEN BEGIN
          RESULT  :=  Lexem.IntVal <= ORD(HIGH(CHAR));
        END; // .IF

        IF NOT RESULT THEN BEGIN
          Lexem.Error :=  Lng[NUMBER_TOO_LARGE];
        END // .IF
        ELSE BEGIN
          IF c = 'H' THEN BEGIN
            Lexem.LexType :=  LEX_INTEGER;
          END // .IF
          ELSE BEGIN
            Lexem.LexType :=  LEX_STRING;
            Lexem.StrVal  :=  OneCharStrings[Lexem.IntVal];
          END; // .ELSE
        END; // .ELSE
      END // .IF
      ELSE BEGIN
        Lexem.Error :=  Lng[INVALID_HEX_VALUE];
      END; // .ELSE
    END; // .FUNCTION ParseHexNumber

    FUNCTION ParseIntegerNumber: BOOLEAN;
    BEGIN
      Lexem.Token   :=  Scanner.GetSubstrAtPos
      (
        Lexem.Pos, Scanner.Pos - Lexem.Pos
      );
      RESULT        :=  SysUtils.TryStrToInt(Lexem.Token, Lexem.IntVal);

      IF RESULT THEN BEGIN
        Lexem.LexType :=  LEX_INTEGER;
      END // .IF
      ELSE BEGIN
        Lexem.Error :=  Lng[NUMBER_TOO_LARGE];
      END; // .ELSE
    END; // .FUNCTION ParseIntegerNumber

  BEGIN
    Scanner.SkipCharset(DIGITS);

    IF Scanner.GetCurrChar(c) AND (c = '.') THEN BEGIN
      RESULT  :=  ParseRealNumber;
    END // .IF
    ELSE BEGIN
      IF Scanner.GetCurrChar(c) AND (c IN (HEXDIGITS + ['H', 'X'])) THEN BEGIN
        RESULT  :=  ParseHexNumber;
      END // .IF
      ELSE BEGIN
        RESULT  :=  ParseIntegerNumber;
      END; // .ELSE          
    END; // .ELSE
  END; // .FUNCTION ParseNumber

  FUNCTION ParseString (QuoteChar: CHAR): BOOLEAN;
  BEGIN
    Scanner.GotoNextChar;
    RESULT  :=  Scanner.FindChar(QuoteChar);

    IF RESULT THEN BEGIN
      Scanner.GotoNextChar;
      Lexem.Token   :=  Scanner.GetSubstrAtPos(Lexem.Pos, Scanner.Pos - Lexem.Pos);
      Lexem.LexType :=  LEX_STRING;

      IF LENGTH(Lexem.Token) = CHAR_TOKEN_LEN THEN BEGIN
        Lexem.StrVal  :=  OneCharStrings[ORD(Lexem.Token[1 + QUOTE_LEN])];
      END // .IF
      ELSE BEGIN
        Lexem.StrVal  :=  COPY(Lexem.Token, 1 + QUOTE_LEN, LENGTH(Lexem.Token) - QUOTES_LEN);
      END; // .ELSE
    END // .IF
    ELSE BEGIN
      Lexem.Error :=  Lng[NO_CLOSING_QUOTE];
    END; // .ELSE
  END; // .FUNCTION ParseString

  FUNCTION ParseComment: BOOLEAN;
  VAR
    NestingLevel: INTEGER;
  
  BEGIN
    Scanner.GotoRelPos(+COMMENT_TAG_LEN);
    NestingLevel  :=  1;
    RESULT        :=  FALSE;
    
    WHILE NOT RESULT AND Scanner.FindCharset(['*', '(']) DO BEGIN
      Scanner.ReadChar(c);
      
      IF c = '*' THEN BEGIN
        IF Scanner.GetCurrChar(c) AND (c = ')') THEN BEGIN
          DEC(NestingLevel);
          Scanner.GotoNextChar;
        END; // .IF
      END // .IF
      ELSE BEGIN
        IF Scanner.GetCurrChar(c) AND (c = '*') THEN BEGIN
          INC(NestingLevel);
          Scanner.GotoNextChar;
        END; // .IF
      END; // .ELSE
      
      RESULT  :=  NestingLevel = 0;
    END; // .WHILE

    IF RESULT THEN BEGIN
      Lexem.Token   :=  Scanner.GetSubstrAtPos
      (
        Lexem.Pos,
        Scanner.Pos - Lexem.Pos
      );
      Lexem.StrVal  :=  COPY
      (
        Lexem.Token,
        1 + COMMENT_TAG_LEN,
        LENGTH(Lexem.Token) - COMMENT_TAGS_LEN
      );
      Lexem.LexType :=  LEX_COMMENT;
    END // .IF
    ELSE BEGIN
      Lexem.Error :=  Lng[UNCLOSED_COMMENT];
    END; // .ELSE
  END; // .FUNCTION ParseComment

BEGIN
  FormatSettings.DecimalSeparator :=  '.';
  Scanner.SkipCharset(BLANKS);
  Lexem.LexType :=  LEX_ERROR;
  Lexem.Pos     :=  Scanner.Pos;
  Lexem.Line    :=  Scanner.LineN;
  Lexem.LinePos :=  Scanner.Pos - Scanner.LineStartPos;
  RESULT        :=  NOT Scanner.EndOfText;

  IF RESULT THEN BEGIN
    Scanner.GetCurrChar(c);

    CASE c OF 
      'a'..'z',
      'A'..'Z',
      '_':        RESULT  :=  ParseIdent;
      '0'..'9':   RESULT  :=  ParseNumber;
      '"', '''':  RESULT  :=  ParseString(c);

      '(':
        BEGIN
          IF Scanner.GetCharAtRelPos(+1, c) AND (c = '*') THEN BEGIN
            RESULT  :=  ParseComment;
          END // .IF
          ELSE BEGIN
            Scanner.GotoNextChar;
            Lexem.LexType :=  LEX_LEFT_PARANTHESIS;
            Lexem.Token   :=  '(';
          END; // .ELSE
        END;

      ':':
        BEGIN
          IF Scanner.GetCharAtRelPos(+1, c) AND (c = '=') THEN BEGIN
            Scanner.GotoRelPos(+2);
            Lexem.Token   :=  ':=';
            Lexem.LexType :=  LEX_BECOMES;
          END // .IF
          ELSE BEGIN
            Scanner.GotoNextChar;
            Lexem.Token   :=  ':';
            Lexem.LexType :=  LEX_COLON;
          END; // .ELSE
        END;

      '<':
        BEGIN
          IF Scanner.GetCharAtRelPos(+1, c) AND (c = '=') THEN BEGIN
            Scanner.GotoRelPos(+2);
            Lexem.Token   :=  '<=';
            Lexem.LexType :=  LEX_LESS_EQUALS;
          END // .IF
          ELSE BEGIN
            Scanner.GotoNextChar;
            Lexem.Token   :=  '<';
            Lexem.LexType :=  LEX_LESS;
          END; // .ELSE
        END;

      '>':
        BEGIN
          IF Scanner.GetCharAtRelPos(+1, c) AND (c = '=') THEN BEGIN
            Scanner.GotoRelPos(+2);
            Lexem.Token   :=  '>=';
            Lexem.LexType :=  LEX_GREATER_EQUALS;
          END // .IF
          ELSE BEGIN
            Scanner.GotoNextChar;
            Lexem.Token   :=  '>';
            Lexem.LexType :=  LEX_GREATER;
          END; // .ELSE
        END;

      '.':
        BEGIN
          IF Scanner.GetCharAtRelPos(+1, c) AND (c = '.') THEN BEGIN
            Scanner.GotoRelPos(+2);
            Lexem.Token   :=  '..';
            Lexem.LexType :=  LEX_DOUBLEDOT;
          END // .IF
          ELSE BEGIN
            Scanner.GotoNextChar;
            Lexem.Token   :=  '.';
            Lexem.LexType :=  LEX_DOT;
          END; // .ELSE
        END;
    ELSE
      Lexem.LexType :=  LexTable[ORD(c)];
      Lexem.Token   :=  c;
      RESULT        :=  Lexem.LexType <> LEX_ERROR;
      Scanner.GotoNextChar;

      IF NOT RESULT THEN BEGIN
        Lexem.Error :=  Lng[ILLEGAL_CHAR];
      END; // .IF
    END; // .SWITCH c
  END // .IF
  ELSE BEGIN
    Lexem.LexType :=  LEX_NO_ITEM;
  END; // .ELSE
END; // .FUNCTION ReadLexem

PROCEDURE InitLexTable;
VAR
  i:  INTEGER;

BEGIN
  FOR i := 0 TO HIGH(LexTable) DO BEGIN
    LexTable[i] :=  LEX_ERROR;
  END; // .FOR

  LexTable[ORD('~')]  :=  LEX_TILDE;
  LexTable[ORD('&')]  :=  LEX_AMPERSAND;
  LexTable[ORD('+')]  :=  LEX_PLUS;
  LexTable[ORD('-')]  :=  LEX_MINUS;
  LexTable[ORD('*')]  :=  LEX_ASTERISK;
  LexTable[ORD('/')]  :=  LEX_SLASH;
  LexTable[ORD(',')]  :=  LEX_COMMA;
  LexTable[ORD(';')]  :=  LEX_SEMICOLON;
  LexTable[ORD('|')]  :=  LEX_VERTBAR;
  LexTable[ORD('^')]  :=  LEX_CARET;
  LexTable[ORD('=')]  :=  LEX_EQUALS;
  LexTable[ORD('#')]  :=  LEX_NOT_EQUALS;
  LexTable[ORD(')')]  :=  LEX_RIGHT_PARANTHESIS;
  LexTable[ORD('[')]  :=  LEX_LEFT_SQUARE_BRACKET;
  LexTable[ORD(']')]  :=  LEX_RIGHT_SQUARE_BRACKET;
  LexTable[ORD('{')]  :=  LEX_LEFT_CURLY_BRACE;
  LexTable[ORD('}')]  :=  LEX_RIGHT_CURLY_BRACE;
END; // .PROCEDURE InitLexTable

PROCEDURE InitOneCharStrings;
VAR
  i:  INTEGER;

BEGIN
  FOR i := 0 TO HIGH(OneCharStrings) DO BEGIN
    OneCharStrings[i] :=  CHR(i);
  END; // .FOR
END; // .PROCEDURE InitOneCharStrings

BEGIN
  Lng :=  @LexerLng.Strs;
  Lang.RegisterClient
  (
    'Lexer',
    Lang.ENG,
    Lang.IS_ANSI,
    ORD(HIGH(LexerLng.TLangStrings)) + 1,
    @Lng,
    Lng
  );

  InitLexTable;
  InitOneCharStrings;
END.

UNIT LexerLng;
{
DESCRIPTION:	Language unit
AUTHOR:				Alexander Shostak (aka Berserker aka EtherniDee aka BerSoft)
}

(***)  INTERFACE   (***)

TYPE
	TLangStrings =
	(
		ILLEGAL_CHAR,
		NUMBER_TOO_LARGE,
		INVALID_HEX_VALUE,
		NO_CLOSING_QUOTE,
		UNCLOSED_COMMENT
	); // TLangStrings

	PLangStringsArr = ^TLangStringsArr;
	TLangStringsArr = ARRAY [TLangStrings] OF STRING;


VAR
	Strs: TLangStringsArr =
	(
		// ILLEGAL_CHAR
		'Illegal character',
		// NUMBER_TOO_LARGE
		'Number too large',
		// INVALID_HEX_VALUE
		'Invalid hexadecimal value',
		// NO_CLOSING_QUOTE
		'Missing closing quote',
		// UNCLOSED_COMMENT
		'Unclosed comment'
	); // Lng


(***) IMPLEMENTATION (***)


END.

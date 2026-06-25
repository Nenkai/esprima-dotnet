#nullable enable

namespace Esprima;

public partial class AdhocAbstractSyntaxTree
{
    private static partial bool IsAssignmentOperator(string input)
    {
        switch (input.Length)
        {
            case 1:
            {
                return input == "=";
            }
            case 2:
            {
                return input[0] switch
                {
                    '-' => input == "-=",
                    '*' => input == "*=",
                    '/' => input == "/=",
                    '&' => input == "&=",
                    '%' => input == "%=",
                    '^' => input == "^=",
                    '+' => input == "+=",
                    '|' => input == "|=",
                    _ => false
                };
            }
            case 3:
            {
                return input[0] switch
                {
                    '?' => input == "??=",
                    '*' => input == "**=",
                    '&' => input == "&&=",
                    '<' => input == "<<=",
                    '>' => input == ">>=",
                    '|' => input == "||=",
                    _ => false
                };
            }
            default:
                return false;
        }
    }

    private static partial bool IsPunctuatorExpressionStart(string input)
    {
        switch (input.Length)
        {
            case 1:
            {
                return input[0] switch
                {
                    '-' => input == "-",
                    '!' => input == "!",
                    '(' => input == "(",
                    '[' => input == "[",
                    '{' => input == "{",
                    '/' => input == "/",
                    '+' => input == "+",
                    '~' => input == "~",
                    _ => false
                };
            }
            case 2:
            {
                return input[0] switch
                {
                    '-' => input == "--",
                    '/' => input == "/=",
                    '+' => input == "++",
                    _ => false
                };
            }
            default:
                return false;
        }
    }

    private static partial bool IsKeywordExpressionStart(string input)
    {
        switch (input.Length)
        {
            case 3:
            {
                return input == "new";
            }
            case 4:
            {
                return input[0] switch
                {
                    't' => input == "this",
                    'v' => input == "void",
                    _ => false
                };
            }
            case 5:
            {
                return input[0] switch
                {
                    'c' => input == "class",
                    's' => input == "super",
                    'y' => input == "yield",
                    _ => false
                };
            }
            case 6:
            {
                return input[0] switch
                {
                    'd' => input == "delete",
                    'i' => input == "import",
                    't' => input == "typeof",
                    _ => false
                };
            }
            case 8:
            {
                return input == "function";
            }
            default:
                return false;
        }
    }
}

internal partial class ParserExtensions
{
    internal static partial string? TryGetInternedString(System.ReadOnlySpan<char> input)
    {
        switch (input.Length)
        {
            case 2:
            {
                return input[1] switch
                {
                    's' => input[0] == 'a' ? "as" : null,
                    'o' => input[0] == 'd' ? "do" : null,
                    'f' => input[0] == 'i' ? "if" : null,
                    'n' => input[0] == 'i' ? "in" : null,
                    _ => null
                };
            }
            case 3:
            {
                return input[0] switch
                {
                    'f' => input[1] == 'o' && input[2] == 'r' ? "for" : null,
                    'k' => input[1] == 'e' && input[2] == 'y' ? "key" : null,
                    'n' => input[1] == 'i' && input[2] == 'l' ? "nil" : null,
                    'o' => input[1] == 'b' && input[2] == 'j' ? "obj" : null,
                    't' => input[1] == 'r' && input[2] == 'y' ? "try" : null,
                    'v' => input[1] == 'a' && input[2] == 'r' ? "var" : null,
                    _ => null
                };
            }
            case 4:
            {
                return input[0] switch
                {
                    'M' => input[1] == 'a' && input[2] == 't' && input[3] == 'h' ? "Math" : null,
                    'a' => input[1] == 'r' && input[2] == 'g' && input[3] == 's' ? "args" : null,
                    'c' => input[1] == 'a' && input[2] == 's' && input[3] == 'e' ? "case" : null,
                    'd' => input[1] switch
                    {
                        'a' => input[2] == 't' && input[3] == 'a' ? "data" : null,
                        'o' => input[2] == 'n' && input[3] == 'e' ? "done" : null,
                        _ => null
                    },
                    'e' => input[1] == 'l' && input[2] == 's' && input[3] == 'e' ? "else" : null,
                    'n' => input[1] == 'a' && input[2] == 'm' && input[3] == 'e' ? "name" : null,
                    's' => input[1] == 'e' && input[2] == 'l' && input[3] == 'f' ? "self" : null,
                    't' => input[1] == 'r' && input[2] == 'u' && input[3] == 'e' ? "true" : null,
                    _ => null
                };
            }
            case 5:
            {
                return input[0] switch
                {
                    'A' => input[1] == 'r' && input[2] == 'r' && input[3] == 'a' && input[4] == 'y' ? "Array" : null,
                    'a' => input[1] switch
                    {
                        's' => input[2] == 'y' && input[3] == 'n' && input[4] == 'c' ? "async" : null,
                        'w' => input[2] == 'a' && input[3] == 'i' && input[4] == 't' ? "await" : null,
                        _ => null
                    },
                    'b' => input[1] == 'r' && input[2] == 'e' && input[3] == 'a' && input[4] == 'k' ? "break" : null,
                    'c' => input[1] switch
                    {
                        'a' => input[2] == 't' && input[3] == 'c' && input[4] == 'h' ? "catch" : null,
                        'l' => input[2] == 'a' && input[3] == 's' && input[4] == 's' ? "class" : null,
                        _ => null
                    },
                    'f' => input[1] == 'a' && input[2] == 'l' && input[3] == 's' && input[4] == 'e' ? "false" : null,
                    'p' => input[1] == 'r' && input[2] == 'i' && input[3] == 'n' && input[4] == 't' ? "print" : null,
                    's' => input[1] == 'u' && input[2] == 'p' && input[3] == 'e' && input[4] == 'r' ? "super" : null,
                    't' => input[1] == 'h' && input[2] == 'r' && input[3] == 'o' && input[4] == 'w' ? "throw" : null,
                    'u' => input[1] == 'n' && input[2] == 'd' && input[3] == 'e' && input[4] == 'f' ? "undef" : null,
                    'v' => input[1] == 'a' && input[2] == 'l' && input[3] == 'u' && input[4] == 'e' ? "value" : null,
                    'w' => input[1] == 'h' && input[2] == 'i' && input[3] == 'l' && input[4] == 'e' ? "while" : null,
                    'y' => input[1] == 'i' && input[2] == 'e' && input[3] == 'l' && input[4] == 'd' ? "yield" : null,
                    _ => null
                };
            }
            case 6:
            {
                return input[0] switch
                {
                    'O' => input[1] == 'b' && input[2] == 'j' && input[3] == 'e' && input[4] == 'c' && input[5] == 't' ? "Object" : null,
                    'S' => input[1] == 'y' && input[2] == 'm' && input[3] == 'b' && input[4] == 'o' && input[5] == 'l' ? "Symbol" : null,
                    'i' => input[1] == 'm' && input[2] == 'p' && input[3] == 'o' && input[4] == 'r' && input[5] == 't' ? "import" : null,
                    'l' => input[1] == 'e' && input[2] == 'n' && input[3] == 'g' && input[4] == 't' && input[5] == 'h' ? "length" : null,
                    'm' => input[1] switch
                    {
                        'e' => input[2] == 't' && input[3] == 'h' && input[4] == 'o' && input[5] == 'd' ? "method" : null,
                        'o' => input[2] == 'd' && input[3] == 'u' && input[4] == 'l' && input[5] == 'e' ? "module" : null,
                        _ => null
                    },
                    'o' => input[1] == 'b' && input[2] == 'j' && input[3] == 'e' && input[4] == 'c' && input[5] == 't' ? "object" : null,
                    'r' => input[1] == 'e' && input[2] == 't' && input[3] == 'u' && input[4] == 'r' && input[5] == 'n' ? "return" : null,
                    's' => input[1] switch
                    {
                        't' => input[2] == 'a' && input[3] == 't' && input[4] == 'i' && input[5] == 'c' ? "static" : null,
                        'w' => input[2] == 'i' && input[3] == 't' && input[4] == 'c' && input[5] == 'h' ? "switch" : null,
                        _ => null
                    },
                    _ => null
                };
            }
            case 7:
            {
                return input[2] switch
                {
                    'f' => input[0] == 'd' && input[1] == 'e' && input[3] == 'a' && input[4] == 'u' && input[5] == 'l' && input[6] == 't' ? "default" : null,
                    'n' => input[0] == 'f' && input[1] == 'i' && input[3] == 'a' && input[4] == 'l' && input[5] == 'l' && input[6] == 'y' ? "finally" : null,
                    'r' => input[0] == 'f' && input[1] == 'o' && input[3] == 'e' && input[4] == 'a' && input[5] == 'c' && input[6] == 'h' ? "foreach" : null,
                    't' => input[0] == 'o' && input[1] == 'p' && input[3] == 'i' && input[4] == 'o' && input[5] == 'n' && input[6] == 's' ? "options" : null,
                    'q' => input[0] == 'r' && input[1] == 'e' && input[3] == 'u' && input[4] == 'i' && input[5] == 'r' && input[6] == 'e' ? "require" : null,
                    _ => null
                };
            }
            case 8:
            {
                return input[0] switch
                {
                    'c' => input[1] == 'o' && input[2] == 'n' && input[3] == 't' && input[4] == 'i' && input[5] == 'n' && input[6] == 'u' && input[7] == 'e' ? "continue" : null,
                    'd' => input[1] == 'e' && input[2] == 'l' && input[3] == 'e' && input[4] == 'g' && input[5] == 'a' && input[6] == 't' && input[7] == 'e' ? "delegate" : null,
                    'f' => input[1] == 'u' && input[2] == 'n' && input[3] == 'c' && input[4] == 't' && input[5] == 'i' && input[6] == 'o' && input[7] == 'n' ? "function" : null,
                    _ => null
                };
            }
            case 9:
            {
                return input[2] switch
                {
                    'g' => input[0] == 'a' && input[1] == 'r' && input[3] == 'u' && input[4] == 'm' && input[5] == 'e' && input[6] == 'n' && input[7] == 't' && input[8] == 's' ? "arguments" : null,
                    't' => input[0] == 'a' && input[1] == 't' && input[3] == 'r' && input[4] == 'i' && input[5] == 'b' && input[6] == 'u' && input[7] == 't' && input[8] == 'e' ? "attribute" : null,
                    'o' => input[0] == 'p' && input[1] == 'r' && input[3] == 't' && input[4] == 'o' && input[5] == 't' && input[6] == 'y' && input[7] == 'p' && input[8] == 'e' ? "prototype" : null,
                    'd' => input[0] == 'u' && input[1] == 'n' && input[3] == 'e' && input[4] == 'f' && input[5] == 'i' && input[6] == 'n' && input[7] == 'e' && input[8] == 'd' ? "undefined" : null,
                    _ => null
                };
            }
            case 10:
            {
                return input.SequenceEqual("use strict".AsSpan()) ? "use strict" : null;
            }
            case 12:
            {
                return input.SequenceEqual("\"use strict\"".AsSpan()) ? "\"use strict\"" : null;
            }
            default:
                return null;
        }
    }
}

public partial class Scanner
{
    private static partial string? TryGetInternedTwoCharacterPunctuator(System.ReadOnlySpan<char> input)
    {
        return input[0] switch
        {
            '!' => input[1] == '=' ? "!=" : null,
            '%' => input[1] == '=' ? "%=" : null,
            '&' => input[1] switch
            {
                '&' => "&&",
                '=' => "&=",
                _ => null
            },
            '*' => input[1] switch
            {
                '*' => "**",
                '=' => "*=",
                _ => null
            },
            '+' => input[1] switch
            {
                '+' => "++",
                '=' => "+=",
                _ => null
            },
            '-' => input[1] switch
            {
                '-' => "--",
                '=' => "-=",
                _ => null
            },
            '/' => input[1] == '=' ? "/=" : null,
            ':' => input[1] == ':' ? "::" : null,
            '<' => input[1] switch
            {
                '<' => "<<",
                '=' => "<=",
                _ => null
            },
            '=' => input[1] switch
            {
                '=' => "==",
                '>' => "=>",
                _ => null
            },
            '>' => input[1] switch
            {
                '=' => ">=",
                '>' => ">>",
                _ => null
            },
            '^' => input[1] == '=' ? "^=" : null,
            '|' => input[1] switch
            {
                '=' => "|=",
                '|' => "||",
                _ => null
            },
            _ => null
        };
    }

    private static partial string? TryGetInternedThreeCharacterPunctuator(System.ReadOnlySpan<char> input)
    {
        return input[0] switch
        {
            '*' => input[1] == '*' && input[2] == '=' ? "**=" : null,
            '&' => input[1] == '&' && input[2] == '=' ? "&&=" : null,
            '<' => input[1] == '<' && input[2] == '=' ? "<<=" : null,
            '>' => input[1] == '>' && input[2] == '=' ? ">>=" : null,
            '|' => input[1] == '|' && input[2] == '=' ? "||=" : null,
            _ => null
        };
    }

    public static partial bool IsKeyword(string input)
    {
        switch (input.Length)
        {
            case 2:
            {
                return input[0] switch
                {
                    'd' => input == "do",
                    'i' => input == "if",
                    _ => false
                };
            }
            case 3:
            {
                return input[0] switch
                {
                    'f' => input == "for",
                    't' => input == "try",
                    'v' => input == "var",
                    _ => false
                };
            }
            case 4:
            {
                return input[0] switch
                {
                    'c' => input == "case",
                    'e' => input == "else",
                    's' => input == "self",
                    _ => false
                };
            }
            case 5:
            {
                return input[4] switch
                {
                    'k' => input == "break",
                    'h' => input == "catch",
                    's' => input == "class",
                    't' => input == "print",
                    'r' => input == "super",
                    'w' => input == "throw",
                    'f' => input == "undef",
                    'e' => input == "while",
                    'd' => input == "yield",
                    _ => false
                };
            }
            case 6:
            {
                return input[5] switch
                {
                    't' => input == "import",
                    'd' => input == "method",
                    'e' => input == "module",
                    'n' => input == "return",
                    'c' => input == "static",
                    'h' => input == "switch",
                    _ => false
                };
            }
            case 7:
            {
                return input[2] switch
                {
                    'f' => input == "default",
                    'n' => input == "finally",
                    'r' => input == "foreach",
                    'q' => input == "require",
                    _ => false
                };
            }
            case 8:
            {
                return input[0] switch
                {
                    'c' => input == "continue",
                    'd' => input == "delegate",
                    'f' => input == "function",
                    _ => false
                };
            }
            case 9:
            {
                return input == "attribute";
            }
            default:
                return false;
        }
    }
}

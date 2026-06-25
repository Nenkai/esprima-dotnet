using System;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Path) })]
public sealed partial class SourceFileStatement : Statement
{
    public Literal Path { get; set; }

    public SourceFileStatement(Literal path) : base(Nodes.SourceFileStatement)
    {
        Path = path;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static SourceFileStatement Rewrite(Literal path)
    {
        return new SourceFileStatement(path);
    }
}

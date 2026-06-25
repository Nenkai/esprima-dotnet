using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text;
using System.Threading.Tasks;
using Esprima.Utils;

namespace Esprima.Ast;

[VisitableNode(ChildProperties = new[] { nameof(Id) })]
public sealed partial class StaticIdentifier : Expression
{
    public StaticIdentifier(Identifier identifier) : base(Nodes.StaticIdentifier)
    {
        Id = identifier;
    }

    public Identifier Id { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static StaticIdentifier Rewrite(Identifier id)
    {
        return new StaticIdentifier(id);
    }
}

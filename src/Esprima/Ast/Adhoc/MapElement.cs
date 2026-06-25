using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast;

// Map/Dictionary for adhoc
[VisitableNode(ChildProperties = new[] { nameof(Key), nameof(Value) })]
public sealed partial class MapElement : Expression
{
    public MapElement(Expression key, Expression value) : base(Nodes.MapExpression)
    {
        Key = key;
        Value = value;
    }

    public Expression Key { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }
    public Expression Value { [MethodImpl(MethodImplOptions.AggressiveInlining)] get; }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static MapElement Rewrite(Expression key, Expression value)
    {
        return new MapElement(key, value);
    }
}

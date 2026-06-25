using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;
using System.Xml.Linq;
using Esprima.Utils;

namespace Esprima.Ast;

// Map/Dictionary for adhoc
[VisitableNode(ChildProperties = new[] { nameof(Elements) })]
public sealed partial class MapExpression : Expression
{
    private readonly NodeList<MapElement> _elements;

    public MapExpression(in NodeList<MapElement> elements) : base(Nodes.MapExpression)
    {
        _elements = elements;
    }

    public ref readonly NodeList<MapElement> Elements => ref _elements;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static MapExpression Rewrite(in NodeList<MapElement> elements)
    {
        return new MapExpression(elements);
    }
}

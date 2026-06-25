using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Esprima.Ast;

public sealed class ObjectSelectorMemberExpression : MemberExpression
{
    public ObjectSelectorMemberExpression(Expression obj, Expression property, bool optional)
        : base(obj, property, true, optional)
    {
    }

    protected override MemberExpression Rewrite(Expression @object, Expression property)
    {
        return new ObjectSelectorMemberExpression(@object, property, Optional);
    }
}

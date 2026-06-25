using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Esprima.Ast;

public sealed class AttributeMemberExpression : MemberExpression
{
    public AttributeMemberExpression(Expression obj, Expression property, bool optional)
        : base(obj, property, false, optional)
    {
    }

    protected override MemberExpression Rewrite(Expression @object, Expression property)
    {
        return new AttributeMemberExpression(@object, property, Optional);
    }
}

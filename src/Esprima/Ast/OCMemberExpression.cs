namespace Esprima.Ast
{
    public sealed class OCMemberExpression : MemberExpression
    {
        public OCMemberExpression(Expression obj, Expression property, bool optional)
            : base(obj, property, false, optional)
        {
        }
    }
}

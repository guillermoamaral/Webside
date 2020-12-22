# Retrieve projects
Retrieve all subprojects of a given root project (if no root is provided, the uppermost project in the system is used).
It is also possible get a tree-like structure as well as to limit the depth in the projects hierarchy.    

**URL**: `/projects`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| ------------- | ------------- | ------------- |
| root | string | the name of the root project |
| names | boolean | true to get only project names |
| tree | boolean | true to get a tree-like structure |
| depth | number | to limit the hierarchy depth (only when tree=true) |

## Success Responses

**Code** : `200 OK`

**Content**: `[project]` where `project` is defined as:
```json
{
    "name": "string",
    "classes": ["string"],
    "methods": [
        {
            "class": "string",
            "selector": "string"
        }
    ]
}
```

**Example:**: `Javascript` subprojects `GET /projects?root=Javascript`.
```json
[
    {
        "name": "Javascript-Compilation-Tests",
        "classes": [],
        "methods": []
    },
    {
        "name": "Javascript-Development",
        "classes": [
            "JSEPrintSourceVisitor",
            "JSEColorPrinterVisitor"
        ],
        "methods": [
            {
                "class": "PPParser",
                "selector": "jseWithLoggingTo:"
            },
            {
                "class": "PPParser",
                "selector": "jseParseAndCountProductions:"
            }
        ]
    },
    {
        "name": "Javascript-Tests",
        "classes": [
            "JavascriptCompilingTest",
            "JavascriptNumericConversionTest",
            "BasicJavascriptParserTest",
            "JavascriptParserTest",
            "JavascriptFormattingTest",
            "JavascriptSyntaxTest",
            "JSETestParserResource",
            "JSETestFactory",
            "JavascriptNodeTest",
            "JavascriptLexicalTest",
            "JavascriptUnicodeTest"
        ],
        "methods": [
            {
                "class": "PPParser",
                "selector": "jseIsLeftRecursiveProduction:"
            },
            {
                "class": "PPSequenceParser",
                "selector": "jseIsLeftRecursiveProduction:"
            },
            {
                "class": "PPParser",
                "selector": "jseIsLeftRecursiveProduction"
            }
        ]
    },
    {
        "name": "Javascript-Parser",
        "classes": [
            "JSEAbstractParser",
            "JSEStrictLexicalParser",
            "JSELexicalParser",
            "JSERegexToken",
            "JSEParsedNodesFactory",
            "JSEToken",
            "JSENilFactory",
            "JSELiteralSequenceChoiceParser",
            "JSEUnicodeTables",
            "JSELookaheadParser",
            "JSECharactersParser",
            "JSEParser",
            "JSETokenParser",
            "JSESyntacticParser",
            "JSESyntaxTreeFactory",
            "JSENumericStringParser"
        ],
        "methods": [
            {
                "class": "PPChoiceParser",
                "selector": "jseIsLeftRecursiveProduction:"
            },
            {
                "class": "SmalltalkSession class",
                "selector": "javascriptParserJustBound"
            },
            {
                "class": "PPParser",
                "selector": "jseToken:"
            },
            {
                "class": "PPParser",
                "selector": "allParsersDo:visited:"
            },
            {
                "class": "PPParser",
                "selector": "listSeparatedBy:"
            },
            {
                "class": "PPParser",
                "selector": "butNot:"
            },
            {
                "class": "String",
                "selector": "jseTokenParser"
            },
            {
                "class": "PPParser",
                "selector": "jseToken:valueBlock:"
            },
            {
                "class": "PPParser",
                "selector": "allParsersDo:"
            },
            {
                "class": "Character",
                "selector": "jseTokenParser"
            }
        ]
    },
    {
        "name": "Javascript-AST",
        "classes": [
            "JSEEnumeratingForNode",
            "JSEForEnumerateVariableNode",
            "JSEUnaryOperatorNode",
            "JSEStringNode",
            "JSEThisNode",
            "JSEPostfixOperatorNode",
            "JSELetDeclarationNode",
            "JSEGetPropertyAssignmentNode",
            "JSEDoWhileNode",
            "JSEGroupedExpressionNode",
            "JSEVariableStatementNode",
            "JSEIdentifierNode",
            "JSEBinaryOperatorNode",
            "JSEPropertyFunctionAssignmentNode",
            "JSEForEnumerateElementsNode",
            "JSEPropertyNode",
            "JSEExceptionHandlerNode",
            "JSEPrimitiveLiteralNode",
            "JSELetStatementNode",
            "JSESwitchClause",
            "JSETryNode",
            "JSECatchNode",
            "JSECallNode",
            "JSEVariableDeclarationNode",
            "JSEParseNode",
            "JSEArrayNode",
            "JSEConditionalIterationNode",
            "JSELiteralNode",
            "JSECommaSeparatedExpressionsNode",
            "JSEThrowNode",
            "JSENullNode",
            "JSEMemberNode",
            "JSEConstStatementNode",
            "JSEProgramNode",
            "JSEConditionalForNode",
            "JSEConditionalNode",
            "JSEIterationNode",
            "JSEDebuggerNode",
            "JSERegularExpressionNode",
            "JSESyntaxTreeVisitor",
            "JSEFunctionExpressionNode",
            "JSEForEnumerateLetNode",
            "JSESetPropertyAssignmentNode",
            "JSESwitchNode",
            "JSEContinueNode",
            "JSEBlockNode",
            "JSEBracketMemberNode",
            "JSEForWithVariableListNode",
            "JSEFinallyNode",
            "JSEConditionalExpressionNode",
            "JSEIfNode",
            "JSEPropertyAssignmentNode",
            "JSEBreakNode",
            "JSEExpressionStatementNode",
            "JSEEncodingVisitor",
            "JSEBooleanNode",
            "JSEObjectNode",
            "JSEFunctionBodyNode",
            "JSEMacroNode",
            "JSEIdentifierNameNode",
            "JSEWithNode",
            "JSEFunctionCallNode",
            "JSEDotMemberNode",
            "JSEAssignmentNode",
            "JSESourceElementListNode",
            "JSEFunctionDeclarationNode",
            "JSESwitchDefaultClause",
            "JSEForWithInitializerNode",
            "JSENewNode",
            "JSEEmptyStatementNode",
            "JSEForWithLetListNode",
            "JSELabelledStatementNode",
            "JSEReturnNode",
            "JSEForIterationNode",
            "JSEOperatorNode",
            "JSEWhileNode",
            "JSEFunctionNode",
            "JSEControlStatementNode",
            "JSENumericNode",
            "JSESwitchClauseNode"
        ],
        "methods": []
    }
]
```
# Retrieve packages
Retrieve all packages in the system.

**URL**: `/packages`

**Method**: `GET`

**Query Options**
| Option | Type | Description |
| ------------- | ------------- | ------------- |
| names | boolean | true to get only package names |

## Success Responses

**Code** : `200 OK`

**Content**: `[package]` where `package` is defined as:
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

**Example:**: `GET /packages`.
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
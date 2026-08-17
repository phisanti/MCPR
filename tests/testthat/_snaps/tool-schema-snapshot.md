# flat type tokens emit a stable JSON Schema

    Code
      cat(emitted_tool_schema(flat_token_tool_code, "flat_token_surface"))
    Output
      {
        "name": "flat_token_surface",
        "description": "Exercises every supported roxygen type token.",
        "inputSchema": {
          "type": "object",
          "properties": {
            "a": {
              "type": "string",
              "description": "A string."
            },
            "b": {
              "type": "string",
              "description": "A character."
            },
            "c": {
              "type": "number",
              "description": "A number."
            },
            "d": {
              "type": "number",
              "description": "A numeric."
            },
            "e": {
              "type": "integer",
              "description": "An integer."
            },
            "f": {
              "type": "integer",
              "description": "An int."
            },
            "g": {
              "type": "boolean",
              "description": "A boolean."
            },
            "h": {
              "type": "boolean",
              "description": "A logical."
            },
            "i": {
              "type": "boolean",
              "description": "A bool."
            },
            "j": {
              "type": "array",
              "description": "An array.",
              "items": {
                "type": "string"
              }
            },
            "k": {
              "type": "object",
              "description": "An object.",
              "additionalProperties": true
            },
            "l": {
              "type": "object",
              "description": "A json object.",
              "additionalProperties": true,
              "x-mcpr-type": "json_object"
            },
            "m": {
              "type": "array",
              "description": "A json array.",
              "x-mcpr-type": "json_array"
            },
            "n": {
              "type": "object",
              "description": "A named list.",
              "additionalProperties": true,
              "x-mcpr-type": "json_object"
            },
            "o": {
              "type": "object",
              "description": "A list.",
              "additionalProperties": true,
              "x-mcpr-type": "json_object"
            }
          },
          "required": ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n"]
        }
      }

# nested object and enum tokens emit a stable JSON Schema

    Code
      cat(emitted_tool_schema(nested_tool_code, "nested_query_surface"))
    Output
      {
        "name": "nested_query_surface",
        "description": "Exercises the nested object declaration syntax.",
        "inputSchema": {
          "type": "object",
          "properties": {
            "query": {
              "type": "object",
              "description": "Structured search request. Terms are OR-matched.",
              "properties": {
                "terms": {
                  "type": "array",
                  "items": {
                    "type": "string"
                  }
                },
                "mode": {
                  "type": "string",
                  "enum": ["auto", "exact", "contains", "regex"]
                },
                "max_hits": {
                  "type": "integer"
                }
              },
              "required": ["terms"],
              "additionalProperties": false
            },
            "rank_by": {
              "type": "string",
              "enum": ["score_desc", "score_asc", "name"],
              "description": "Sort key for the result table."
            }
          },
          "required": ["query"]
        }
      }


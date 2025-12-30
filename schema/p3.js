module.exports = {
    schema: {
        "type": "json_schema",
        "json_schema": {
        "name": "classification",
        "strict": true,
        "schema": {
            "type": "object",
            "properties": {      
                "belief": {
                    "type": "string",
                    "description": "level-0 belief mean",
                    "enum": [                        
                        "0",
                        "16",
                        "26",
                        "36",
                        "46",
                        "56",
                        "66",
                        "76",
                        "na"
                    ]
                },    
                "level": {
                    "type": "string",
                    "description": "level of strategic thinking as described in ",
                    "enum": [
                        "0",
                        "1",
                        "2",
                        "3",
                        "4",
                        "5",
                        "eq",
                        "na"
                    ]
                }
            },
            "required": [
                "belief",
                "level"
            ],
            "additionalProperties": false
        }
    }
  }
}
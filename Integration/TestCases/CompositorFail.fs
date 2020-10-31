module Core.TestCases.CompositorFail

open Integration
open Microsoft.FSharpLu.Json
open NUnit.Framework
open FsUnit
open Twime
open Twime.UI

let testCase = Compact.deserialize """
{
  "Displays": [
    {
      "Reference": {
        "TreeRef": 5
      },
      "Meta": {
        "Name": "\\\\.\\DISPLAY2",
        "WorkArea": {
          "Left": -1400,
          "Right": -40,
          "Top": -807,
          "Bottom": 1673
        },
        "ActiveTag": {
          "TreeRef": 1
        },
        "Primary": false,
        "Active": false
      },
      "Tags": [
        {
          "Reference": {
            "TreeRef": 1
          },
          "Meta": {
            "Name": "1"
          },
          "Layout": {
            "ContainerNode": [
              {
                "TreeRef": 2
              },
              {
                "LayoutEngine": "vertical"
              },
              [
                {
                  "WindowNode": [
                    {
                      "TreeRef": 11
                    },
                    {
                      "Name": "PuTTY (inactive)",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": -1380,
                          "Right": -60,
                          "Top": -785,
                          "Bottom": -198
                        },
                        "title": "PuTTY (inactive)",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 6822142
                          }
                        }
                      }
                    }
                  ]
                },
                {
                  "WindowNode": [
                    {
                      "TreeRef": 14
                    },
                    {
                      "Name": "You're Being Hunted  -  Professor Elemental  -  The Giddy Limit  -  2014",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": -1380,
                          "Right": -60,
                          "Top": -176,
                          "Bottom": 422
                        },
                        "title": "You're Being Hunted  -  Professor Elemental  -  The Giddy Limit  -  2014",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 5573058
                          }
                        }
                      }
                    }
                  ]
                },
                {
                  "WindowNode": [
                    {
                      "TreeRef": 15
                    },
                    {
                      "Name": "Telegram",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": -1380,
                          "Right": -60,
                          "Top": 444,
                          "Bottom": 1042
                        },
                        "title": "Telegram",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 5114856
                          }
                        }
                      }
                    }
                  ]
                },
                {
                  "WindowNode": [
                    {
                      "TreeRef": 17
                    },
                    {
                      "Name": "@Enika - Discord",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": -1380,
                          "Right": -60,
                          "Top": 1064,
                          "Bottom": 1651
                        },
                        "title": "@Enika - Discord",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 7143912
                          }
                        }
                      }
                    }
                  ]
                }
              ]
            ]
          }
        },
        {
          "Reference": {
            "TreeRef": 3
          },
          "Meta": {
            "Name": "2"
          },
          "Layout": {
            "ContainerNode": [
              {
                "TreeRef": 4
              },
              {
                "LayoutEngine": "vertical"
              },
              []
            ]
          }
        }
      ]
    },
    {
      "Reference": {
        "TreeRef": 10
      },
      "Meta": {
        "Name": "\\\\.\\DISPLAY1",
        "WorkArea": {
          "Left": 40,
          "Right": 3400,
          "Top": 70,
          "Bottom": 1315
        },
        "ActiveTag": {
          "TreeRef": 8
        },
        "Primary": true,
        "Active": true
      },
      "Tags": [
        {
          "Reference": {
            "TreeRef": 6
          },
          "Meta": {
            "Name": "1"
          },
          "Layout": {
            "ContainerNode": [
              {
                "TreeRef": 7
              },
              {
                "LayoutEngine": "horizontal"
              },
              [
                {
                  "WindowNode": [
                    {
                      "TreeRef": 12
                    },
                    {
                      "Name": "logs",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": 60,
                          "Right": 1150,
                          "Top": 92,
                          "Bottom": 1293
                        },
                        "title": "logs",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 5048074
                          }
                        }
                      }
                    }
                  ]
                },
                {
                  "WindowNode": [
                    {
                      "TreeRef": 18
                    },
                    {
                      "Name": "reddit: the front page of the internet - Mozilla Firefox",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": 1170,
                          "Right": 2270,
                          "Top": 92,
                          "Bottom": 1293
                        },
                        "title": "Home - Mozilla Firefox",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 17435630
                          }
                        }
                      }
                    }
                  ]
                },
                {
                  "WindowNode": [
                    {
                      "TreeRef": 19
                    },
                    {
                      "Name": "TWiME-2 – Program.fs",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": 2290,
                          "Right": 3380,
                          "Top": 92,
                          "Bottom": 1293
                        },
                        "title": "TWiME-2 – Program.fs",
                        "minimised": false,
                        "active": true,
                        "handle": {
                          "WindowHandle": {
                            "value": 56757334
                          }
                        }
                      }
                    }
                  ]
                }
              ]
            ]
          }
        },
        {
          "Reference": {
            "TreeRef": 8
          },
          "Meta": {
            "Name": "2"
          },
          "Layout": {
            "ContainerNode": [
              {
                "TreeRef": 9
              },
              {
                "LayoutEngine": "horizontal"
              },
              []
            ]
          }
        }
      ]
    }
  ]
}
"""

[<Test>]
let ``Test case should correctly composit`` () =
    let compositor = Compositor.composit 50 NoUI.ui (Twime.LayoutPostProcessors.NoPostProcessor.postprocess) (Twime.LayoutPostProcessors.NoPostProcessor.postprocess) ()
    
    testCase
    |> TreeTagOperation.setActiveTag "2"
    |> Option.get
    |> compositor 
    |> should ofCase <@ Some @>

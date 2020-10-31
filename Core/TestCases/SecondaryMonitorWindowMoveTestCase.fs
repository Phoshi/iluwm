module Core.TestCases.SecondaryMonitorWindowMoveTestCase

open Microsoft.FSharpLu.Json
open Twime
let tree = Compact.deserialize<TwimeRoot.T> """
{
  "Displays": [
    {
      "Reference": {
        "TreeRef": 5
      },
      "Meta": {
        "Name": "\\\\.\\DISPLAY2",
        "WorkArea": {
          "Left": -1440,
          "Right": 0,
          "Top": -847,
          "Bottom": 1713
        },
        "ActiveTag": {
          "TreeRef": 1
        },
        "Primary": false,
        "Active": true
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
                  "ContainerNode": [
                    {
                      "TreeRef": 27
                    },
                    {
                      "LayoutEngine": "horizontal"
                    },
                    [
                      {
                        "WindowNode": [
                          {
                            "TreeRef": 11
                          },
                          {
                            "Name": "Cloud 9 - Chilled + Distilled (vol. 1)",
                            "Weight": {
                              "Horizontal": 1.0,
                              "Vertical": 1.0
                            },
                            "Definition": {
                              "size": {
                                "Left": -1440,
                                "Right": 0,
                                "Top": -847,
                                "Bottom": -207
                              },
                              "title": "Cloud 9 - Chilled + Distilled (vol. 1)",
                              "minimised": false,
                              "active": false,
                              "handle": {
                                "WindowHandle": {
                                  "value": 264618
                                }
                              }
                            }
                          }
                        ]
                      }
                    ]
                  ]
                },
                {
                  "WindowNode": [
                    {
                      "TreeRef": 12
                    },
                    {
                      "Name": "Mafia Town Is Asleep  -  Pascal Michael Stiefel  -  A Hat in Time OST  -  2017",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": -1440,
                          "Right": 0,
                          "Top": -207,
                          "Bottom": 433
                        },
                        "title": "Mafia Town Is Asleep  -  Pascal Michael Stiefel  -  A Hat in Time OST  -  2017",
                        "minimised": false,
                        "active": true,
                        "handle": {
                          "WindowHandle": {
                            "value": 263996
                          }
                        }
                      }
                    }
                  ]
                },
                {
                  "WindowNode": [
                    {
                      "TreeRef": 13
                    },
                    {
                      "Name": "#general - Discord",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": -1440,
                          "Right": 0,
                          "Top": 433,
                          "Bottom": 1073
                        },
                        "title": "#general - Discord",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 197604
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
                          "Left": -1440,
                          "Right": 0,
                          "Top": 1073,
                          "Bottom": 1713
                        },
                        "title": "Telegram",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 198672
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
          "Left": 0,
          "Right": 3440,
          "Top": 30,
          "Bottom": 1355
        },
        "ActiveTag": {
          "TreeRef": 6
        },
        "Primary": true,
        "Active": false
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
                      "TreeRef": 14
                    },
                    {
                      "Name": "Ergodox EZ Configurator - Mozilla Firefox",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": 0,
                          "Right": 2150,
                          "Top": 30,
                          "Bottom": 1355
                        },
                        "title": "Ergodox EZ Configurator - Mozilla Firefox",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 133498
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
                      "Name": "TWiME-2 – Program.fs",
                      "Weight": {
                        "Horizontal": 0.6000000000000001,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": 2150,
                          "Right": 3440,
                          "Top": 30,
                          "Bottom": 1355
                        },
                        "title": "TWiME-2 – Program.fs",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 1509648
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
              [
                {
                  "WindowNode": [
                    {
                      "TreeRef": 18
                    },
                    {
                      "Name": "Libraries",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": 0,
                          "Right": 1720,
                          "Top": 30,
                          "Bottom": 1355
                        },
                        "title": "Fallout VR Essentials",
                        "minimised": false,
                        "active": true,
                        "handle": {
                          "WindowHandle": {
                            "value": 39193228
                          }
                        }
                      }
                    }
                  ]
                },
                {
                  "WindowNode": [
                    {
                      "TreeRef": 23
                    },
                    {
                      "Name": "Fallout 4 VR Mod Organizer v2.2.2.1 (Phoshi*)",
                      "Weight": {
                        "Horizontal": 1.0,
                        "Vertical": 1.0
                      },
                      "Definition": {
                        "size": {
                          "Left": 1720,
                          "Right": 3440,
                          "Top": 30,
                          "Bottom": 1355
                        },
                        "title": "Fallout 4 VR Mod Organizer v2.2.2.1 (Phoshi*)",
                        "minimised": false,
                        "active": false,
                        "handle": {
                          "WindowHandle": {
                            "value": 2690652
                          }
                        }
                      }
                    }
                  ]
                }
              ]
            ]
          }
        }
      ]
    }
  ]
}
"""

open NUnit.Framework
open FsUnit

[<Test; Ignore "Old tree format">]
let ``Attempting to move the active window should work`` () =
  TreeMoveOperation.moveActiveWindow TreeMove.Up tree
  |> should ofCase <@ Some @>
import React, { Component } from "react";
import { Box, TextField, Typography, Slider } from "@mui/material";

class ColorEditor extends Component {
    constructor(props) {
        super(props);
        const rgba = this.props.value || "#ffffff00";
        this.state = {
            rgb: this.rgbFrom(rgba),
            transparency: this.transparencyFrom(rgba),
        };
    }

    rgbChanged(rgb) {
        this.setState({ rgb: rgb });
        if (this.props.onChange)
            this.props.onChange(this.rgba(rgb, this.state.transparency));
    }

    transparencyChanged(transparency) {
        this.setState({ transparency: transparency });
        if (this.props.onChange)
            this.props.onChange(this.rgba(this.state.rgb, transparency));
    }

    rgba(rgb, transparency) {
        const percent = Math.round(((100 - transparency) * 255) / 100);
        const a = percent.toString(16).padStart(2, 0);
        return (rgb + a).toLowerCase();
    }

    rgbFrom(rgba) {
        return rgba.substring(0, 7);
    }

    transparencyFrom(rgba) {
        return rgba.length > 7
            ? 100 -
                  Math.round(
                      (Number("0x" + rgba.substring(7, 9)) / 255) * 100.0
                  )
            : 0;
    }

    render() {
        const { rgb, transparency } = this.state;
        const { name, editable } = this.props;
        const textColor = editable ? "text.primary" : "grey.500";
        return (
            <Box display="flex" flexDirection="column" sx={{ gap: 1 }}>
                <Box display="flex" alignItems="center" sx={{ gap: 2 }}>
                    <TextField
                        sx={{
                            width: 50,
                            height: 35,
                            "& .MuiOutlinedInput-notchedOutline": {
                                border: "2px solid",
                                borderColor: "divider",
                                borderRadius: 1,
                            },
                            "& .MuiOutlinedInput-root": {
                                padding: 0,
                                height: 35,
                            },
                            '& input[type="color"]': {
                                width: "100%",
                                height: 35,
                                padding: 0,
                                margin: 0,
                                border: "none",
                                borderRadius: 1,
                                cursor: editable ? "pointer" : "default",
                                appearance: "none",
                                "&::-webkit-color-swatch-wrapper": {
                                    padding: 0,
                                },
                                "&::-webkit-color-swatch": {
                                    border: "none",
                                    borderRadius: 1,
                                },
                            },
                        }}
                        size="small"
                        id={name}
                        type="color"
                        name={name}
                        variant="outlined"
                        value={rgb}
                        onChange={(event) => {
                            this.rgbChanged(event.target.value);
                        }}
                        disabled={!editable}
                    />
                    <Box
                        display="flex"
                        flexDirection="column"
                        sx={{ gap: 0.5 }}
                    >
                        <Typography
                            variant="body2"
                            sx={{
                                color: "text.secondary",
                                fontSize: "0.75rem",
                            }}
                        >
                            {rgb.toUpperCase()}
                        </Typography>
                        <Box display="flex" alignItems="center" sx={{ gap: 2 }}>
                            <Typography
                                variant="caption"
                                sx={{ color: textColor, minWidth: 80 }}
                            >
                                Transparency
                            </Typography>
                            <Box sx={{ width: 100 }}>
                                <Slider
                                    value={transparency}
                                    onChange={(event, value) => {
                                        this.transparencyChanged(value);
                                    }}
                                    valueLabelDisplay="auto"
                                    valueLabelFormat={(value) => `${value}%`}
                                    size="small"
                                    disabled={!editable}
                                />
                            </Box>
                        </Box>
                    </Box>
                </Box>
            </Box>
        );
    }
}

export default ColorEditor;

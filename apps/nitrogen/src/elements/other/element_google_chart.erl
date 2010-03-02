% Nitrogen Web Framework for Erlang
% Copyright (c) 2008-2010 Rusty Klophaus
% See MIT-LICENSE for licensing information.

-module (element_google_chart).
-compile(export_all).
-include_lib ("wf.hrl").
-include_lib ("google_chart.hrl").

reflect() -> record_info(fields, google_chart).

render_element(Record) -> 
    % Path...
    Path = "http://chart.apis.google.com/chart?",

    % Chart Type...
    Type = [
        "&cht=",
        case Record#google_chart.type of
            line -> "lc";
            sparkline -> "ls";
            stacked_horizontal_bar -> "bhs";
            stacked_vertical_bar -> "bvs";
            grouped_horizontal_bar -> "bhg";
            grouped_vertical_bar -> "bvg";
            pie -> "p";
            pie3d -> "p3";
            OtherType -> erlang:error({unknown_chart_type, OtherType})
        end
    ],

    % Title...
    Title = case Record#google_chart.title of
        undefined -> [];
        [] -> [];
        OtherTitle -> ["&chtt=", wf:url_encode(OtherTitle)]
    end,

    % Title Color and Font Size...
    TitleStyle = wf:f("&chts=~s,~b", [wf:to_list(Record#google_chart.color), Record#google_chart.font_size]),

    % Size...
    Size = wf:f("&chs=~bx~b", [Record#google_chart.width, Record#google_chart.height]),

    % Grid...
    Grid = wf:f("&chg=~s,~s,~b,~b", [
        wf:to_list(wf:coalesce([Record#google_chart.grid_x, 0])),
        wf:to_list(wf:coalesce([Record#google_chart.grid_y, 0])),
        Record#google_chart.grid_line_length,
        Record#google_chart.grid_blank_length
    ]),

    % Background Colors...
    BGColors = wf:f("&chf=bg,s,~s|c,s,~s", [
        wf:to_list(Record#google_chart.background_color), 
        wf:to_list(Record#google_chart.chart_color)
    ]),

    % Legend Location...
    LegendLocation = "&chdlp=" ++ case Record#google_chart.legend_location of
        top -> "t";
        left -> "l";
        bottom -> "b";
        right -> "r"
    end,

    % Axes...
    Axes = case Record#google_chart.axes of 
        undefined -> [];
        [] -> [];
        AxesRecords ->			
            ProcessedAxes = [process_axis(N - 1, lists:nth(N, AxesRecords)) || N <- lists:seq(1, length(AxesRecords))],
            AxesPositions = "&chxt=" ++ string:join([X || [X, _, _] <- ProcessedAxes], ","),
            AxesLabels    = "&chxl=" ++ string:join([X || [_, X, _] <- ProcessedAxes], "|"),
            AxesColors    = "&chxs=" ++ string:join([X || [_, _, X] <- ProcessedAxes], "|"),
            AxesPositions ++ AxesLabels ++ AxesColors
    end,

    % Data...
    Data = case Record#google_chart.data of
        undefined -> MaxValueLength=0, [];
        [] -> MaxValueLength=0, [];
        DataRecords ->
            ProcessedData = [process_data(N -1, lists:nth(N, DataRecords)) || N <- lists:seq(1, length(DataRecords))],
            DataColors  = "&chco="  ++ string:join([X || [X, _, _, _, _, _] <- ProcessedData], ","),
            DataLegends = "&chdl="  ++ string:join([X || [_, X, _, _, _, _] <- ProcessedData], "|"),
            DataScales  = "&chds="  ++ string:join([X || [_, _, X, _, _, _] <- ProcessedData], ","),
            DataStyles  = "&chls="  ++ string:join([X || [_, _, _, X, _, _] <- ProcessedData], "|"),
            DataValues  = "&chd=t:" ++ string:join([X || [_, _, _, _, X, _] <- ProcessedData], "|"),
            MaxValueLength = lists:max([X || [_, _, _, _, _, X] <- ProcessedData]),
            DataLegends1 = case string:strip(DataLegends, both, $|) of
                "&chdl=" -> [];
                _ -> DataLegends
            end,				

            DataColors ++ DataLegends1 ++ DataScales ++ DataValues ++ DataStyles
    end,

    % Calculate bar size...
    BarSize = case MaxValueLength of 
        0 -> [];
        _ -> 
            DataGroupsLength = length(Record#google_chart.data),
            BarGroupSpace = Record#google_chart.bar_group_space,
            BarSpace = Record#google_chart.bar_space,
            GroupSpacerPixels = MaxValueLength * BarGroupSpace,
            BarSpacerPixels = MaxValueLength * (DataGroupsLength * BarSpace),
            AvailablePixels = case Record#google_chart.type of 
                stacked_horizontal_bar -> Record#google_chart.height;
                grouped_horizontal_bar -> Record#google_chart.height;
                stacked_vertical_bar -> Record#google_chart.width;
                grouped_vertical_bar -> Record#google_chart.width;
                _ -> 0
            end,
            IndividualBarSize = (AvailablePixels - GroupSpacerPixels - BarSpacerPixels) / (DataGroupsLength * MaxValueLength),
            wf:f("&chbh=~b,~b,~b", [trunc(IndividualBarSize), BarSpace, BarGroupSpace])
    end,

    % Render the image tag...
    Image = #image {
        id=Record#google_chart.id,
        anchor=Record#google_chart.anchor,
        class=[google_chart, Record#google_chart.class],
        style = Record#google_chart.style,
        image = lists:flatten([Path, Type, Title, TitleStyle, Size, Grid, BGColors, LegendLocation, BarSize, Axes, Data])
    },
    element_image:render_element(Image).

process_axis(N, Axis) ->
    Position = case Axis#chart_axis.position of
        top -> "t";
        right -> "r";
        bottom -> "x";
        left -> "y";
        OtherPosition -> erlang:error({unknown_axis_position, OtherPosition})
    end,
    StringLabels = [wf:to_list(X) || X <- Axis#chart_axis.labels],
    Labels = integer_to_list(N) ++ ":|" ++ string:join(StringLabels, "|"),
    Style = wf:f("~b,~s,~b", [N, wf:to_list(Axis#chart_axis.color), Axis#chart_axis.font_size]),
    [Position, Labels, Style].

process_data(_N, Data) ->
    Color = wf:to_list(Data#chart_data.color),
    Legend = wf:to_list(Data#chart_data.legend),
    Scale = wf:f("~b,~b", [Data#chart_data.min_value, Data#chart_data.max_value]),
    StringValues = [wf:to_list(X) || X <- Data#chart_data.values],
    Values = string:join(StringValues, ","),
    Styles = wf:f("~b,~b,~b", [Data#chart_data.line_width, Data#chart_data.line_length, Data#chart_data.blank_length]),
    [Color, Legend, Scale, Styles, Values, length(StringValues)].


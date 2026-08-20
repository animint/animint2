function bin_index_for_stat_bin(x, breaks) {
        var i, left, right;
        for (i = 0; i < breaks.length - 1; i++) {
                left = breaks[i];
                right = breaks[i + 1];
                if (i === 0) {
                        if (left <= x && x <= right) {
                                return i;
                        }
                } else if (left < x && x <= right) {
                        return i;
                }
        }
        return -1;
}

function count_into_breaks(rows, breaks) {
        var n_bins = breaks.length - 1;
        var counts = [];
        var i, row_i, bin_i, x_val;
        for (i = 0; i < n_bins; i++) {
                counts[i] = 0;
        }
        for (row_i = 0; row_i < rows.length; row_i++) {
                x_val = rows[row_i].x;
                if (x_val === null || isNaN(x_val)) {
                        continue;
                }
                bin_i = bin_index_for_stat_bin(Number(x_val), breaks);
                if (0 <= bin_i) {
                        counts[bin_i] += 1;
                }
        }
        return counts;
}

function compute_stat_bin(data, params, PANEL) {
        var breaks, panel_key, groups, out, group_i, group_rows;
        var fill_val, counts, bin_i;
        if (!Array.isArray(data) || data.length === 0) {
                return [];
        }
        var panel_index = Number(PANEL) - 1;
        breaks = params.x_breaks[panel_index];
        if ((!breaks || breaks.length < 2) && params.x_breaks.hasOwnProperty(String(PANEL))) {
                breaks = params.x_breaks[String(PANEL)];
        }
        if (!breaks || breaks.length < 2) {
                return [];
        }
        groups = d3.nest().key(function(d) {
                return d.group;
        }).sortKeys(d3.ascending).entries(data);
        out = [];
        for (group_i = 0; group_i < groups.length; group_i++) {
                group_rows = groups[group_i].values;
                fill_val = group_rows[0].fill;
                counts = count_into_breaks(group_rows, breaks);
                for (bin_i = 0; bin_i < counts.length; bin_i++) {
                        out.push({
                                xmin: breaks[bin_i],
                                xmax: breaks[bin_i + 1],
                                ymin: 0,
                                ymax: counts[bin_i],
                                fill: fill_val,
                                group: groups[group_i].key
                        });
                }
        }
        return out;
}

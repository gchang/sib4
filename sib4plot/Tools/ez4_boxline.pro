pro ez4_boxline, x0, y0, x1, y1, color=color, thick=thick
    oplot, [x0, x1], [y0, y0], color=color, thick=thick
    oplot, [x1, x1], [y0, y1], color=color, thick=thick
    oplot, [x0, x1], [y1, y1], color=color, thick=thick
    oplot, [x0, x0], [y0, y1], color=color, thick=thick
end

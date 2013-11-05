This version:

*   adds HTTP error codes for missing items;
*   does path normalization to prevent users fetching files that aren't
    below the specified root directory; and
*   has some logging built in.

If nothing corresponds to the requested path, a 404 (File Not Found)
error is returned.  If the thing corresponding to the path can't be
read, a 403 (No Permissions) error is returned.  If something else
goes wrong, a 500 (Internal) error is returned.

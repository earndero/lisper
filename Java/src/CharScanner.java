public class CharScanner {
    String s;
    int position = 0;
    int line = 0;

    CharScanner(String s) {
        this.s = s;
    }

    boolean nextChar() {
        if (position<s.length()) {
            position++;
            if (isLineBreak(peek())) return false;
            return true;
        }
        else return false;
    }

    static boolean is_space(char ch) {
        return Character.isSpaceChar(ch) || ch==10 || ch==13 || ch=='\t';
    }

    public void skip_whitespace() {
        while (position<s.length() && is_space(s.charAt(position))) { position++; }
    }

    private void skipEols() {
        while (position<s.length() && isLineBreak(s.charAt(position))) { position++; }
    }

    public char peek() {
        if (eof())
            return 0;
        else
            return s.charAt(position);
    }

    public void skip_comments() {
        while (position<s.length() && !isLineBreak(s.charAt(position)))
            position++;
    }

    boolean eof() {
        return position>=s.length();
    }

    boolean eof(int la) {//todo to remove
        return position+la>=s.length();
    }

    public char peek(int la) {
        if (eof(la))
            return 0;
        else
            return s.charAt(position+la);
    }

    int anchor = 0;

    boolean isLineBreak(char c) {
        return c==10 ||c==13;
    }

    void setAnchor() {
        this.anchor = position;
    }

    String getAnchor() {
        return s.substring(anchor, position);
    }

    public void skip_nocode() {
        while (true) {
            skip_whitespace();
            if (eof()) return;
            if (peek() == ';') skip_comments();
            else if (isLineBreak(peek())) skipEols();
            else break;
        }
    }


}

/*
* */
public class CharScanner {
    String s;
    int position = 0;
    int line = 0;

    CharScanner(String s) {
        this.s = s;
    }

    void nextChar() {
        position++;
    }

    static boolean is_space(char ch) {
        return Character.isSpaceChar(ch) || ch==10 || ch==13;
    }

    public void skip_whitespace() {
        while (position<s.length() && is_space(s.charAt(position))) { position++; }
    }

    public char peek() {
        if (eof())
            return 0;
        else
            return s.charAt(position);
    }

    public char peek(int la) {
        return s.charAt(position+1);
    }

    public void skip_comments() {
        while (position<s.length() && s.charAt(position)!='\n')
            position++;
        if (s.charAt(position)=='\n')position++;
    }

    boolean eof() {
        return position>=s.length();
    }

    int anchor = 0;

    void setAnchor() {
        this.anchor = position;
    }

    String getAnchor() {
        return s.substring(anchor, position);
    }
}

using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace lab3_CSharp {
    class Program {

        public class OutOfRangeException : System.Exception {
            public OutOfRangeException() : base() {}
        }

        abstract class Set {

            public int MaxVal;

            public abstract void AddEl(int val);
            public abstract void EraseEl(int val);
            public abstract bool CheckEl(int val);

            public Set(int maxVal) {
                MaxVal = maxVal;
            }

            public void Build(string s) {
                int cur = 0;
                for (int i = 0; i < s.Length; i++) {
                    if (s[i] == ' ') {
                        if (cur > 0) {
                            AddEl(cur);
                            cur = 0;
                        }
                    }
                    else {
                        cur = cur * 10 + s[i] - '0';
                    }
                }
                if (cur > 0) {
                    AddEl(cur);
                }
            }

            public void Build(int[] a) {
                for (int i = 0; i < a.Length; i++)
                    AddEl(a[i]);
            }

            public void Print() {
                for (int i = 1; i <= MaxVal; i++)
                    if (CheckEl(i))
                        Console.Write(i.ToString() + " ");
                Console.Write("\n");
            }
        }

        class SimpleSet : Set {
            bool[] m;

            public SimpleSet(int maxVal) : base(maxVal) {
                m = new bool[maxVal];
            }

            override public void AddEl(int val) {
                if (val > MaxVal) {
                    throw new OutOfRangeException();
                }
                m[val - 1] = true;
            }

            override public void EraseEl(int val) {
                if (val <= MaxVal && val > 0)
                    m[val - 1] = false;
            }

            override public bool CheckEl(int val) {
                if (val > MaxVal || val < 0) return false;
                return m[val - 1];
            }

            public static SimpleSet operator + (SimpleSet a, SimpleSet b) {
                SimpleSet ans = new SimpleSet(Math.Max(a.MaxVal, b.MaxVal));
                for (int i = 1; i <= Math.Max(a.MaxVal, b.MaxVal); i++)
                    if (i <= a.MaxVal && a.CheckEl(i) || i <= b.MaxVal && b.CheckEl(i))
                        ans.AddEl(i);
                return ans;
            }

            public static SimpleSet operator * (SimpleSet a, SimpleSet b) {
                SimpleSet ans = new SimpleSet(Math.Min(a.MaxVal, b.MaxVal));
                for (int i = 1; i <= Math.Min(a.MaxVal, b.MaxVal); i++)
                    if (a.CheckEl(i) && b.CheckEl(i))
                        ans.AddEl(i);
                return ans;
            }
        }

        class Bitset : Set {
            int[] m;
            public Bitset(int maxVal) : base(maxVal) {
                m = new int[(maxVal + 31) / 32];
            }

            public override void AddEl(int val) {
                if (val > MaxVal) {
                    throw new OutOfRangeException();
                }
                val--;
                int ind = val / 32;
                int pos = val % 32;
                m[ind] |= (1 << pos);
            }

            public override void EraseEl(int val) {
                if (val > MaxVal || val <= 0) return;
                val--;
                int ind = val / 32;
                int pos = val % 32;
                if ((m[ind] | (1 << pos)) > 0)
                    m[ind] ^= (1 << pos);
            }

            public override bool CheckEl(int val) {
                if (val <= 0 || val > MaxVal) return false;
                val--;
                int ind = val / 32;
                int pos = val % 32;
                return (m[ind] & (1 << pos)) > 0;
            }

            public static Bitset operator +(Bitset a, Bitset b) {
                Bitset ans = new Bitset(Math.Max(a.MaxVal, b.MaxVal));
                for (int i = 1; i <= Math.Max(a.MaxVal, b.MaxVal); i++)
                    if (i <= a.MaxVal && a.CheckEl(i) || i <= b.MaxVal && b.CheckEl(i))
                        ans.AddEl(i);
                return ans;
            }

            public static Bitset operator *(Bitset a, Bitset b) {
                Bitset ans = new Bitset(Math.Min(a.MaxVal, b.MaxVal));
                for (int i = 1; i <= Math.Min(a.MaxVal, b.MaxVal); i++)
                    if (a.CheckEl(i) && b.CheckEl(i))
                        ans.AddEl(i);
                return ans;
            }
        }

        class Multiset : Set {
            int[] m;
            public Multiset(int maxVal) : base(maxVal) {
                m = new int[maxVal];
            }

            public override void AddEl(int val) {
                if (val > MaxVal) {
                    throw new OutOfRangeException();
                }
                m[val - 1]++;
            }

            public override void EraseEl(int val) {
                if (val > 0 && val < MaxVal)
                    m[val - 1] = Math.Max(m[val - 1] - 1, 0);
            }

            public override bool CheckEl(int val) {
                if (val <= 0 || val > MaxVal) return false;
                return m[val - 1] > 0;
            }
        }

        public static void solve2() {
            int N = 100;
            Console.WriteLine("Print two sets");
            SimpleSet a1 = new SimpleSet(N);
            SimpleSet b1 = new SimpleSet(N);
            Bitset a2 = new Bitset(N);
            Bitset b2 = new Bitset(N);
            string st = Console.ReadLine();
            a1.Build(st);
            a2.Build(st);
            st = Console.ReadLine();
            b1.Build(st);
            b2.Build(st);
            var c1 = a1 + b1;
            c1.Print();
            var d1 = a1 * b1;
            d1.Print();
            var c2 = a2 + b2;
            c2.Print();
            var d2 = a2 * b2;
            d2.Print();
        }

        public static void solve1() {
            Console.WriteLine("Print set type: 1 - Simple set, 2 - Multiset, 3 - Bitset");
            int ans = Convert.ToInt32(Console.ReadLine());
            Console.WriteLine("Print max value");
            int maxVal = Convert.ToInt32(Console.ReadLine());
            Set s;
            if (ans == 1)
                s = new SimpleSet(maxVal);
            else if (ans == 2)
                s = new Multiset(maxVal);
            else
                s = new Bitset(maxVal);
            Console.WriteLine("Initialize set: 1 - From console, 2 - From file");
            ans = Convert.ToInt32(Console.ReadLine());
            try {
                if (ans == 1) {
                    Console.WriteLine("Print string");
                    string st = Console.ReadLine();
                    s.Build(st);
                }
                else {
                    Console.WriteLine("Print file");
                    string name = Console.ReadLine();
                    FileStream file1 = new FileStream(name, FileMode.Open);
                    StreamReader reader = new StreamReader(file1);
                    var ar = new List<int>();
                    while (!reader.EndOfStream) {
                        int tmp = Convert.ToInt32(reader.ReadLine());
                        ar.Add(tmp);
                    }
                    int[] a = new int[ar.Count];
                    for (int i = 0; i < ar.Count; i++)
                        a[i] = ar[i];
                    s.Build(a);
                }
            }
            catch (OutOfRangeException e) {
                Console.WriteLine("Out of range");
            }

            Console.WriteLine("Operations:\n1 - Add\n2 - Erase\n3 - Check\n4 - Print set\n5 - Exit");

            while (true) {
                Console.WriteLine("Print operation");
                int typ = Convert.ToInt32(Console.ReadLine());
                int ansx = -1;
                if (typ <= 3) {
                    Console.WriteLine("Print number");
                    ansx = Convert.ToInt32(Console.ReadLine());
                }
                if (typ == 1) {
                    try {
                        s.AddEl(ansx);
                    }
                    catch (OutOfRangeException e) {
                        Console.WriteLine("Out of range");
                    }
                }
                else if (typ == 2) {
                    s.EraseEl(ansx);
                }
                else if (typ == 3) {
                    if (s.CheckEl(ansx))
                        Console.WriteLine("Yes");
                    else
                        Console.WriteLine("No");
                }
                else if (typ == 4)
                    s.Print();
                else if (typ == 5) break;
            }
        }


        static void Main(string[] args) {
            solve1();
            //solve2();            
        }
    }
}

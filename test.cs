using System;
using System.IO;
using System.ComponentModel;
using System.Net.Sockets;
using System.Net;
using System.Xml;
using System.Linq;
using static System.Windows.Forms.LinkLabel;
using System.Text;
using static System.Windows.Forms.VisualStyles.VisualStyleElement;



namespace Affarsystem
{
    //Ago Ribic Grundversion
    public partial class LagerKontroll : UserControl
    {
        public BindingList<Bok> Bocker = new BindingList<Bok>();
        public BindingList<Dataspel> Spel = new BindingList<Dataspel>();
        public BindingList<Film> Filmer = new BindingList<Film>();

        public LagerKontroll()
        {
            InitializeComponent();
            BokDataGrid.DataSource = Bocker;
            FilmerDataGrid.DataSource = Filmer;
            SpelDataGrid.DataSource = Spel;

            ProduktValBox.Items.AddRange(new string[] { "Böcker", "Dataspel", "Filmer" });
            ProduktValBox.SelectedIndexChanged += ProduktValBox_SelectedIndexChanged;
            ProduktValBox.SelectedIndex = 0;
        }

        private void ProduktValBox_SelectedIndexChanged(object sender, EventArgs e)
        {
            string val = ProduktValBox.SelectedItem.ToString();

            AuthorBox.Visible = false;
            GenreBox.Visible = false;
            SprakBox.Visible = false;
            FormatBox.Visible = false;
            SpeltidBox.Visible = false;
            SpeltidLabel.Visible = false;

            if (val == "Böcker")
            {
                AuthorBox.Visible = true;
                GenreBox.Visible = true;
                SprakBox.Visible = true;
                FormatBox.Visible = true;
                Genre.Visible = true;

                Author.Text = "Författare";
                Genre.Text = "Genre";
                Sprak.Text = "Språk";
                FormatLabel.Text = "Format";
            }

            else if (val == "Dataspel")
            {
                AuthorBox.Visible = true;
                GenreBox.Visible = false;
                SprakBox.Visible = false;

                Genre.Text = "";
                Sprak.Text = "";
                FormatLabel.Text = "";
                Author.Text = "Plattform";
            }

            else if (val == "Filmer")
            {
                AuthorBox.Visible = true;
                SpeltidBox.Visible = true;
                SpeltidLabel.Visible = true;

                Author.Text = "Format";
                SpeltidLabel.Text = "Speltid";
                Sprak.Text = "";
                FormatLabel.Text = "";
            }
        }

        public void LäggTillProdukt_Click(object sender, EventArgs e)
        {
            string kategori = ProduktValBox.SelectedItem.ToString();
            int? speltid = null;
            if (TitelBox.Text == "")
            {
                MessageBox.Show("Skriv in namn till produkten");
                TitelBox.Clear();
                return;
            }

            if (!int.TryParse(ISBNBox.Text, out int isbn) || isbn < 0)
            {
                MessageBox.Show("Ange en siffra för ISBN");
                return;
            }

            if (int.TryParse(SprakBox.Text, out int språk))
            {
                MessageBox.Show("Ange ett språk");
                return;
            }

            if (int.TryParse(AuthorBox.Text, out int author))
            {
                MessageBox.Show("Författare kan inte vara en siffra");
                return;
            }

            if (int.TryParse(GenreBox.Text, out int genre))
            {
                MessageBox.Show("Genre kan inte vara en siffra");
                return;
            }

            if (int.TryParse(FormatBox.Text, out int format))
            {
                MessageBox.Show("Ange format (pocket, inbunden, etc)");
                return;
            }


            if (!string.IsNullOrEmpty(SpeltidBox.Text))
            {
                if (!int.TryParse(SpeltidBox.Text, out int temp) || temp < 0)
                {
                    MessageBox.Show("Ange ett heltal som speltid");
                    return;
                }
                speltid = temp;
            }


            if (!int.TryParse(PrisBox.Text, out int pris) || pris < 0)
            {
                MessageBox.Show("Ange ett heltal som pris.");
                return;
            }

            if (!int.TryParse(LagerBox.Text, out int lager) || lager < 0)
            {
                MessageBox.Show("Felaktigt antal i lager");
                return;
            }


            bool dublett = false;
            foreach (var Film in Filmer)
            {
                if (Film.ISBN == isbn)
                {
                    dublett = true;
                    MessageBox.Show("ISBN finns redan, välj nytt");
                    ISBNBox.Clear();
                    return;
                }
            }
            foreach (var Bok in Bocker)
            {
                if (Bok.ISBN == isbn)
                {
                    dublett = true;
                    MessageBox.Show("ISBN finns redan, välj nytt");
                    ISBNBox.Clear();
                    return;
                }
            }
            foreach (var dataspel in Spel)
            {
                if (dataspel.ISBN == isbn)
                {
                    dublett = true;
                    MessageBox.Show("ISBN finns redan, välj nytt");
                    ISBNBox.Clear();
                    return;
                }
            }

            if (kategori == "Böcker")
            {
                Bok bok = new Bok
                {
                    Namn = TitelBox.Text,
                    Pris = pris,
                    Författare = AuthorBox.Text,
                    Lager = lager,
                    Genre = GenreBox.Text,
                    ISBN = isbn,
                    Sprak = SprakBox.Text,
                    Format = FormatBox.Text
                };

                Bocker.Add(bok);
            }

            else if (kategori == "Dataspel")
            {
                Dataspel dataspel = new Dataspel
                {
                    Namn = TitelBox.Text,
                    Pris = pris,
                    Lager = lager,
                    ISBN = isbn,
                    Plattform = AuthorBox.Text
                };

                Spel.Add(dataspel);
            }

            else if (kategori == "Filmer")
            {
                Film film = new Film
                {
                    Namn = TitelBox.Text,
                    Pris = pris,
                    Format = AuthorBox.Text,
                    Speltid = speltid,
                    ISBN = isbn,
                    Lager = lager
                };

                Filmer.Add(film);
            }
            Rensa();
        }

        private void Rensa()
        {
            TitelBox.Clear();
            PrisBox.Clear();
            AuthorBox.Clear();
            SpeltidBox.Clear();
            FormatBox.Clear();
            GenreBox.Clear();
            SprakBox.Clear();
            LagerBox.Clear();
            ISBNBox.Clear();
        }



        private void TaBortProdukt_Click(object sender, EventArgs e)
        {

            if (BokDataGrid.SelectedRows.Count > 0)
            {
                Bok? bok = BokDataGrid.SelectedRows[0].DataBoundItem as Bok;
                if (bok != null)
                {
                    if (bok.Lager > 0)
                    {
                        var val = MessageBox.Show("Vill du ta bort vara från lager?", "Dialogruta", MessageBoxButtons.YesNo);
                        if (val == DialogResult.Yes)
                        {
                            Bocker.Remove(bok);
                        }
                    }
                    else
                    {
                        Bocker.Remove(bok);
                    }
                }
            }

            else if (SpelDataGrid.SelectedRows.Count > 0)
            {
                Dataspel? spel = SpelDataGrid.SelectedRows[0].DataBoundItem as Dataspel;
                if (spel != null)
                {
                    if (spel.Lager > 0)
                    {
                        var val = MessageBox.Show("Vill du ta bort vara från lager?", "Dialogruta", MessageBoxButtons.YesNo);
                        if (val == DialogResult.Yes)
                        {
                            Spel.Remove(spel);
                        }
                    }
                    else
                    {
                        Spel.Remove(spel);
                    }
                }
            }

            else if (FilmerDataGrid.SelectedRows.Count > 0)
            {
                Film? film = FilmerDataGrid.SelectedRows[0].DataBoundItem as Film;
                if (film != null)
                {
                    if (film.Lager > 0)
                    {

                        var val = MessageBox.Show("Vill du ta bort vara från lager?", "Dialogruta", MessageBoxButtons.YesNo);
                        if (val == DialogResult.Yes)
                        {
                            Filmer.Remove(film);
                        }
                    }

                    else
                    {
                        Filmer.Remove(film);
                    }
                }
            }

            else
            {
                MessageBox.Show("Välj en produkt att ta bort.");
            }
        }

        public void SparaCSV(string filePath)
        {
            List<string> rader = new List<string>();

            foreach (var bok in Bocker)
            {
                rader.Add($"Bok;{bok.Namn};{bok.Pris};{bok.Författare};{bok.Genre};{bok.Sprak};{bok.ISBN};{bok.Lager};{bok.Format}");
            }

            foreach (var spel in Spel)
            {
                rader.Add($"Dataspel;{spel.Namn};{spel.Pris};{spel.Plattform};{spel.ISBN};{spel.Lager}");
            }

            foreach (var film in Filmer)
            {
                rader.Add($"Film;{film.Namn};{film.Pris};{film.Format};{film.Speltid};{film.ISBN};{film.Lager}");
            }

            File.WriteAllLines(filePath, rader);
        }

        public void TaFramCSV(string filePath)
        {
            if (!File.Exists(filePath))
                return;

            Bocker.Clear();
            Spel.Clear();
            Filmer.Clear();

            var rader = File.ReadAllLines(filePath);

            foreach (var rad in rader)
            {
                var data = rad.Split(';');

                if (data[0] == "Bok")
                {
                    Bocker.Add(new Bok
                    {
                        Namn = data[1],
                        Pris = int.Parse(data[2]),
                        Författare = data[3],
                        Genre = data[4],
                        Sprak = data[5],
                        ISBN = int.Parse(data[6]),
                        Lager = int.Parse(data[7]),
                        Format = data[8]
                    });
                }
                else if (data[0] == "Dataspel")
                {
                    Spel.Add(new Dataspel
                    {
                        Namn = data[1],
                        Pris = int.Parse(data[2]),
                        Plattform = data[3],
                        ISBN = int.Parse(data[4]),
                        Lager = int.Parse(data[5])
                    });
                }
                else if (data[0] == "Film")
                {
                    int? speltid = null;
                    if (data[4] != "")
                    {
                        speltid = int.Parse(data[4]);
                    }

                    Filmer.Add(new Film
                    {
                        Namn = data[1],
                        Pris = int.Parse(data[2]),
                        Format = data[3],
                        Speltid = speltid,
                        ISBN = int.Parse(data[5]),
                        Lager = int.Parse(data[6])
                    });
                }
            }
        }

        private string filePath = Path.Combine(Application.StartupPath, "produkter.csv");

        private void LeveransButton_Click(object sender, EventArgs e)
        {

            if (!int.TryParse(ISBNLeveransBox.Text, out int isbn))
            {
                MessageBox.Show("Ange ett ISBN som finns i lager");
                return;
            }

            string namn = NamnLeveransBox.Text;
            if (string.IsNullOrWhiteSpace(namn))
            {
                MessageBox.Show("Ange ett namn");
                return;
            }

            if (!int.TryParse(AntalLeveransBox.Text, out int antal) || antal <= 0)
            {
                MessageBox.Show("Ange ett positivt heltal");
                return;
            }

            string Check = namn;

            Bok bok = null;
            foreach (var bocker in Bocker)
            {
                if (bocker.ISBN == isbn && bocker.Namn == Check)
                {
                    bok = bocker;
                    break;
                }
            }

            Dataspel dataspel = null;
            foreach (var spel in Spel)
            {
                if (spel.ISBN == isbn && spel.Namn == Check)
                {
                    dataspel = spel;
                    break;
                }
            }

            Film film = null;
            foreach (var filmer in Filmer)
            {
                if (filmer.ISBN == isbn && filmer.Namn == Check)
                {
                    film = filmer;
                    break;
                }
            }

            if (bok == null && dataspel == null && film == null)
            {
                MessageBox.Show("Ingen produkt med vald ISBN och namn hittades.");
                return;
            }

            if (bok != null)
            {
                bok.Lager += antal;
                BokDataGrid.Refresh();
            }
            if (film != null)
            {
                film.Lager += antal;
                FilmerDataGrid.Refresh();
            }
            if (dataspel != null)
            {
                dataspel.Lager += antal;
                SpelDataGrid.Refresh();
            }

            SparaCSV(filePath);
            MessageBox.Show($"Leverans utförd");

            ISBNLeveransBox.Clear();
            AntalLeveransBox.Clear();
            NamnLeveransBox.Clear();
        }

        private void uppdatera_Click(object sender, EventArgs e)
        {
            string API = " https://hex.cse.kau.se/~jonavest/csharp-api/";

            try
            {
                WebClient client = new WebClient();
                string text = client.DownloadString(API);

                XmlDocument doc = new XmlDocument();
                doc.LoadXml(text);

                var kategorier = new (string kategori, IEnumerable<Produkter> produktlista)[]
                {
                    ("book",  Bocker.Cast<Produkter>()),
                    ("game",  Spel.Cast<Produkter>()),
                    ("movie", Filmer.Cast<Produkter>())
                };

                foreach (var (kategori, produktlista) in kategorier)
                {
                    XmlNodeList element = doc.GetElementsByTagName(kategori);

                    foreach (XmlNode nod in element)
                    {
                        int id = int.Parse(nod["id"].InnerText);
                        int pris = int.Parse(nod["price"].InnerText);
                        int lager = int.Parse(nod["stock"].InnerText);

                        foreach (var prod in produktlista)
                        {
                            if (prod.ISBN == id)
                            {
                                prod.Pris = pris;
                                prod.Lager = lager;
                            }
                        }
                    }
                }
                MessageBox.Show("Great Success");
            }
            catch (Exception ex)
            {
                MessageBox.Show($"Ett fel har inträffat {ex.Message}");
            }

            BokDataGrid.Refresh();
            SpelDataGrid.Refresh();
            FilmerDataGrid.Refresh();
        }
    }
}
import string
import re
import pandas as pd
from urllib.request import urlopen
from xml.etree import ElementTree as ET

target_destination = 'data/diamonds_ja.csv'


class JamesAllenDiamondScraper():
    def __init__(self, carat_low: float, carat_high: float, color_list: list,
                 price_low: float, price_high: float):
        self.url_base = 'https://www.jamesallen.com/JSite/Core/jx.ashx?PageUrl='
        self.product_type = 'loose-diamonds/'
        self.diamond_type = 'all-diamonds/'
        self.page_selection = 'page-{}/?'
        self.carat_selection = 'CaratFrom={}%.CaratTo={}%.'.format(
            carat_low, carat_high)
        self.color_selection = 'Color={}%.'.format(','.join(color_list))
        self.price_selection = 'PriceFrom={}%.PriceTo={}%.'.format(
            price_low, price_high)
        self.url_end = 'ViewsOptions=Images&CashedTemplates=,&Container=%23TempResults&Ondemand=True&RequestNum=0&_=1522474926211'

        self.conditional_url = (self.url_base + self.product_type +
                                self.diamond_type + self.page_selection +
                                self.carat_selection + self.color_selection +
                                self.price_selection + self.url_end)

    def parse_diamond_data(self, tree_element):
        ''' Function to parse the xml elements to dictionary
        '''

        base_url = 'https://www.jamesallen.com/'
        diamond_data = dict()
        try:
            diamond_data['id'] = tree_element.get('DiamondID')
            diamond_data['shape'] = tree_element.get('Shape')
            diamond_data['carat'] = float(tree_element.get('Carat'))
            diamond_data['color'] = tree_element.get('Color')
            diamond_data['clarity'] = tree_element.get('Clarity')
            diamond_data['depth'] = float(tree_element.get('Depth'))
            diamond_data['tablesize'] = tree_element.get('TableSize')
            diamond_data['polish'] = tree_element.get('Polish')
            diamond_data['symmetry'] = tree_element.get('Symmetry')
            diamond_data['price'] = float(tree_element.get('Price'))
            diamond_data['lab'] = tree_element.get('Lab')
            diamond_data['hascert'] = tree_element.get('HasCert')
            x, y, z = re.split('\*|x|\-|X',
                               re.sub('\(|\)', '',
                                      tree_element.get('Measurement')))
            diamond_data['x'] = float(x)
            diamond_data['y'] = float(y)
            diamond_data['z'] = float(z)
            diamond_data['cut'] = tree_element.get('Cut')
            diamond_data['url'] = base_url + tree_element.get('Url')
        except:
            pass
        return diamond_data

    def extract_diamond_data(self):
        '''Method to extract the diamond data.

        The method loopds through the pages build with the conditional
        url, then uses the parser to parse the xml element into
        dictionaries.

        When there are no more elements to be parsed, the loop is
        terminated.

        '''

        diamond_data = list()
        last_page = False
        page = 0
        while not last_page:
            current_page = self.conditional_url.format(page)
            try:
                tree = ET.parse(urlopen(current_page))
                root = tree.getroot().findall('./DataSource/DataRef')
                cdata = root[0].text
                xml_data = ET.fromstring(cdata)
                diamond_elements = (xml_data
                                    .findall('SearchResults')[0]
                                    .find('L').getchildren())

                if len(diamond_elements) == 0:
                    last_page = True

                page_data = [self.parse_diamond_data(
                    e) for e in diamond_elements]
                diamond_data += page_data
            except:
                print(current_page)
            page += 1
        return pd.DataFrame(diamond_data)


if __name__ == '__main__':
    # Maximum selection
    carat_low = 0.05
    carat_high = 30
    color_list = list(string.ascii_uppercase[3:])
    price_low = 200
    price_high = 4999000

    scraper = JamesAllenDiamondScraper(carat_low=carat_low,
                                       carat_high=carat_high,
                                       color_list=color_list,
                                       price_low=price_low,
                                       price_high=price_high)

    diamond_data = scraper.extract_diamond_data()
    diamond_data.to_csv(target_destination, index=False)

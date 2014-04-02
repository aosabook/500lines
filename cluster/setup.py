from setuptools import setup, find_packages

setup(
    name='fleet',
    version='1.0.0',
    description='Manage cluster membership',
    author='Dustin J. Mitchell',
    author_email='dustin@cs.uchicago.edu',
    url='',
    install_requires=[],
    packages=find_packages(),
    entry_points="""
    [console_scripts]
        fleet = fleet.run:main
    """,
    extras_require = { 
        'test': [
            'nose',
            'mock'
        ]   
    },  
)
